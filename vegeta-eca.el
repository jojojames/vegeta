;;; vegeta-eca.el --- ECA (Editor Code Assistant) provider for vegeta -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: eca, agent, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Lists and resumes chats stored by ECA (the Editor Code Assistant
;; client/server) in its per-workspace cache:
;;
;;   ~/.cache/eca/<workspace-slug>/
;;     chats/index.transit.json         - the workspace root + per-chat summaries
;;     chats/<chat-uuid>.transit.json   - one full chat (Transit-JSON)
;;     db.transit.json                  - legacy single-file store
;;
;; The cache directory name is `<basename>_<opaque-hash>` (e.g.
;; `james_syt_5DVe' for `/Users/james', `.emacs.d_MGz0b6-H' for
;; `/Users/james/.emacs.d'), so the real workspace path cannot be
;; recovered from the directory name.  The workspace root is stored
;; once per index in `chats/index.transit.json' under `:workspaces', and
;; every chat in that directory shares it -- that root becomes the
;; entry's `:repo', so ECA chats group under their workspace.
;;
;; Discovery and metadata come from the ECA server binary's own reader:
;;
;;   eca read-chat --db-cache-path <dir>                 ; JSONL chat summaries
;;   eca read-chat --db-cache-path <dir> --chat-id <id>  ; JSONL messages
;;
;; We deliberately do NOT hand-roll a Transit decoder: the index is only
;; consulted for the workspace root (a literal, top-level `:workspaces'
;; array), and everything else comes from `read-chat'.
;;
;; RET resumes the chat inside ECA (starting it if needed); `o' or
;; prefix RET renders a read-only transcript from `read-chat'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'vegeta-core)

;; eca is loaded lazily inside `vegeta--eca-visit'.  Its internals are
;; only reached when the package is actually available, so keep them out
;; of the top-level requires (a minimal `async' worker has no eca).

(declare-function eca "eca" (&optional arg))
(declare-function eca-session "eca" ())
(declare-function eca--session-status "eca" (session))
(declare-function eca--session-chats "eca" (session))
(declare-function eca-chat--get-chat-buffer "eca-chat" (session chat-id))
(declare-function eca-chat--begin-opening "eca-chat" (session chat-id))
(declare-function eca-chat--finish-opening "eca-chat" (session chat-id))
(declare-function eca-chat--handle-open-response "eca-chat" (session from-buffer chat-id response))
(declare-function eca-api-request-async "eca-api" (session &rest args))
(declare-function eca-api-request-sync "eca-api" (session &rest args))
(defvar eca-chat-history-page-size)
(defvar eca-after-initialize-hook)

;;; Customization

(defcustom vegeta-eca-cache-dir
  (expand-file-name "~/.cache/eca/")
  "Root directory where ECA keeps its per-workspace chat caches.
One subdirectory per workspace, each holding a `chats/' dir."
  :type 'directory
  :group 'vegeta)

(defcustom vegeta-eca-server-command
  (or (let ((p (expand-file-name "~/.emacs.d/eca/eca")))
        (and (file-executable-p p) p))
      (executable-find "eca")
      "eca")
  "ECA server binary used to read chats (`eca read-chat').
Defaults to the path ECA's own client installs to, then to `eca' on
`exec-path'.  Set to nil to disable the provider."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'vegeta)

;;; Helpers

(defun vegeta--eca-command ()
  "Return the resolved ECA server binary, or nil when unavailable."
  (cond
   ((null vegeta-eca-server-command) nil)
   ((file-executable-p vegeta-eca-server-command)
    vegeta-eca-server-command)
   ((executable-find vegeta-eca-server-command))))

(defun vegeta--eca--run (args)
  "Run the ECA binary with ARGS, returning output split into lines.
Returns nil when the binary is missing or exits non-zero."
  (when-let* ((cmd (vegeta--eca-command)))
    (with-temp-buffer
      (let ((status (apply #'call-process cmd nil t nil args)))
        (when (and (integerp status) (zerop status))
          (split-string (buffer-string) "\n" t))))))

(defun vegeta--eca--jsonl (lines)
  "Parse LINES (JSONL) into a list of plists, skipping malformed lines."
  (delq nil
        (mapcar
         (lambda (line)
           (condition-case nil
               (json-parse-string line
                                  :object-type 'plist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)
             (error nil)))
         lines)))

(defun vegeta--eca-cache-dirs ()
  "Return the ECA per-workspace cache directories.
A directory qualifies when it holds a `chats/' subdirectory (modern
layout) or a `db.transit.json' (legacy layout).  Hidden directories are
included, since ECA's slug for a dot-directory starts with a dot."
  (when (file-directory-p vegeta-eca-cache-dir)
    (seq-filter
     (lambda (f)
       (and (file-directory-p f)
            (or (file-directory-p (expand-file-name "chats" f))
                (file-exists-p (expand-file-name "db.transit.json" f)))))
     (seq-remove (lambda (f)
                   (member (file-name-nondirectory f) '("." "..")))
                 (directory-files vegeta-eca-cache-dir t)))))

(defun vegeta--eca-index-workspace (dir)
  "Return the workspace root recorded in DIR's chat index, or nil.
Reads only the literal top-level `:workspaces' array from
`chats/index.transit.json' -- no general Transit decoding."
  (let ((index (expand-file-name "chats/index.transit.json" dir)))
    (when (file-readable-p index)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents index)
            (goto-char (point-min))
            (when (search-forward "\"~:workspaces\"" nil t)
              (skip-chars-forward " \t\r\n,")
              (when (eq (char-after) ?\[)
                (let ((roots (json-parse-buffer
                              :array-type 'list
                              :object-type 'plist
                              :null-object nil
                              :false-object nil)))
                  (and (listp roots) (car roots))))))
        (error nil)))))

;;; Provider: :list

(defun vegeta--eca-entry (dir workspace chat)
  "Build a vegeta entry for CHAT (a `read-chat' summary plist) in DIR.
WORKSPACE is the workspace root shared by every chat in DIR."
  (let* ((id (plist-get chat :id))
         (file (and id (expand-file-name
                        (concat "chats/" id ".transit.json") dir)))
         (exists (and file (file-exists-p file)))
         (mtime (or (and exists
                         (float-time (file-attribute-modification-time
                                      (file-attributes file))))
                    (and (numberp (plist-get chat :updated-at))
                         (/ (plist-get chat :updated-at) 1000.0))
                    (and (numberp (plist-get chat :created-at))
                         (/ (plist-get chat :created-at) 1000.0))
                    ;; `vegeta--cached-meta' treats a nil `:mtime' as
                    ;; uncacheable, which would leave the row a permanent
                    ;; placeholder; never leave it nil.
                    0.0)))
    (list :provider 'eca
          :id (if exists file id)
          :repo workspace
          :agent (or (plist-get chat :model) "ECA")
          :mtime mtime
          :extras (list :chat-id id
                        :workspace workspace
                        :cache-dir dir
                        :model (plist-get chat :model)
                        :title (plist-get chat :title)
                        :status (plist-get chat :status)
                        :created-ms (plist-get chat :created-at)
                        :updated-ms (plist-get chat :updated-at)))))

(defun vegeta--eca-list ()
  "List every chat in every ECA workspace cache.
In per-chat-layout directories a chat is skipped when its
`.transit.json' is missing: ECA's index can outlive the storage (e.g. a
chat deleted out from under it), and such rows would list but fail to
open.  Legacy `db.transit.json' directories have no per-chat files and
are kept as-is."
  (when (vegeta--eca-command)
    (let (entries)
      (dolist (dir (vegeta--eca-cache-dirs))
        (let* ((per-chat (file-directory-p (expand-file-name "chats" dir)))
               (workspace (vegeta--eca-index-workspace dir))
               (chats (vegeta--eca--jsonl
                       (vegeta--eca--run
                        (list "read-chat" "--db-cache-path" dir)))))
          (dolist (chat chats)
            (let ((id (plist-get chat :id)))
              (when (or (not per-chat)
                        (file-exists-p
                         (expand-file-name (concat "chats/" id ".transit.json")
                                           dir)))
                (push (vegeta--eca-entry dir workspace chat) entries))))))
      entries)))

;;; Provider: :parse

(defun vegeta--eca-ms-to-iso (ms)
  "Format epoch milliseconds MS as an ISO-8601 local timestamp string."
  (when (numberp ms)
    (format-time-string "%Y-%m-%dT%H:%M:%S" (seconds-to-time (/ ms 1000.0)))))

(defun vegeta--eca-parse (entry)
  "Return metadata for ECA ENTRY from the summary captured at list time."
  (let* ((ex (plist-get entry :extras))
         (workspace (plist-get ex :workspace)))
    (list :agent (or (plist-get ex :model) "ECA")
          :model (plist-get ex :model)
          :started-at (vegeta--eca-ms-to-iso (plist-get ex :created-ms))
          :updated-at (vegeta--eca-ms-to-iso (plist-get ex :updated-ms))
          :cwd workspace
          :session-id (plist-get ex :chat-id)
          :first-prompt nil
          :ai-title (plist-get ex :title)
          :renamed nil)))

;;; Provider: :date-key

(defun vegeta--eca-date-key (entry)
  "Return YYYY-MM-DD for ENTRY from its start time (list-time, stable)."
  (let ((ms (plist-get (plist-get entry :extras) :created-ms)))
    (if (numberp ms)
        (format-time-string "%Y-%m-%d" (seconds-to-time (/ ms 1000.0)))
      (vegeta--default-date-key entry))))

;;; Transcript rendering

(defun vegeta--eca-content-text (content)
  "Extract displayable text from a message CONTENT value, or nil.
CONTENT is a string, or a list of `{:type ... :text ...}' maps."
  (cond
   ((stringp content) content)
   ((listp content)
    (let (parts)
      (dolist (item content)
        (let ((text (and (listp item) (plist-get item :text))))
          (when (and (stringp text) (not (string-empty-p text)))
            (push text parts))))
      (when parts (string-join (nreverse parts) "\n"))))
   (t nil)))

(defun vegeta--eca-insert-message (rec)
  "Insert one `read-chat' message REC into the current buffer."
  (let* ((role (or (plist-get rec :role) "?"))
         (ms (plist-get rec :created-at))
         (text (vegeta--eca-content-text (plist-get rec :content)))
         (stamp (if (numberp ms)
                    (format-time-string "%Y-%m-%d %H:%M"
                                        (seconds-to-time (/ ms 1000.0)))
                  "")))
    (insert (propertize (format "%-14s %s\n" role stamp)
                        'face 'vegeta-agent-face))
    (when text (insert text "\n"))
    (insert "\n")))

(defun vegeta--eca-open-transcript (entry)
  "Render ENTRY's ECA chat into a read-only transcript buffer.
Returns the buffer so `vegeta-open-transcript' can display it."
  (let* ((ex (plist-get entry :extras))
         (chat-id (plist-get ex :chat-id))
         (dir (plist-get ex :cache-dir))
         (title (or (plist-get ex :title)
                    (and chat-id (substring chat-id 0 8))
                    "chat"))
         (buffer (get-buffer-create (format "*eca-transcript: %s*" title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "ECA chat %s\n" (or chat-id "?")) 'face 'bold))
        (insert (format "%s\n\n"
                        (or (plist-get ex :workspace) "(unknown workspace)")))
        (if (null (vegeta--eca-command))
            (insert "eca server binary not found; cannot render transcript.\n")
          (dolist (rec (vegeta--eca--jsonl
                        (vegeta--eca--run
                         (list "read-chat" "--db-cache-path" dir
                               "--chat-id" chat-id))))
            (vegeta--eca-insert-message rec))))
      (goto-char (point-min))
      (special-mode))
    buffer))

;;; Provider: :visit

(defun vegeta--eca--open (session chat-id from-buffer)
  "Open CHAT-ID in running SESSION, resuming it as SESSION's chat buffer."
  (eca-chat--begin-opening session chat-id)
  (eca-api-request-async
   session
   :method "chat/open"
   :params (append (list :chatId chat-id)
                   (when (bound-and-true-p eca-chat-history-page-size)
                     (list :limit eca-chat-history-page-size)))
   :success-callback
   (lambda (response)
     ;; `eca-chat--handle-open-response' signals a `user-error' when the
     ;; server no longer has the chat (e.g. a stale row).  Catch it here
     ;; so it can't escape into the process filter.
     (condition-case err
         (eca-chat--handle-open-response session from-buffer chat-id response)
       (error
        (eca-chat--finish-opening session chat-id)
        (message "vegeta[eca]: %s" (error-message-string err)))))
   :error-callback
   (lambda (err)
     (eca-chat--finish-opening session chat-id)
     (message "vegeta[eca]: failed to open chat %s: %s" chat-id err))))

(defun vegeta--eca-visit (entry)
  "Visit ENTRY: resume its ECA chat, or show a transcript as a fallback.
When ECA is already running the chat is opened (or its live buffer
focused); otherwise ECA is started and the chat opened once it is ready.
If the `eca' package is unavailable, a read-only transcript is shown."
  (let* ((ex (plist-get entry :extras))
         (chat-id (plist-get ex :chat-id))
         (from-buffer (current-buffer))
         (session (and (require 'eca nil t)
                       (fboundp 'eca-session)
                       (ignore-errors (eca-session)))))
    (cond
     ;; A buffer for this chat is already open -- just focus it.
     ((and session
           (fboundp 'eca-chat--get-chat-buffer)
           (ignore-errors (eca-chat--get-chat-buffer session chat-id)))
      (vegeta--pop-to (eca-chat--get-chat-buffer session chat-id)))
     ;; ECA is running -- open the chat.
     ((and session
           (fboundp 'eca-chat--begin-opening)
           (fboundp 'eca-chat--handle-open-response)
           (fboundp 'eca-api-request-async)
           (eq (ignore-errors (eca--session-status session)) 'started))
      (vegeta--eca--open session chat-id from-buffer))
     ;; ECA is installed but not running -- start it, then open the chat.
     ((fboundp 'eca)
      (letrec ((fn (lambda ()
                     (remove-hook 'eca-after-initialize-hook fn)
                     (let ((s (ignore-errors (eca-session))))
                       (when (and s (fboundp 'eca-chat--begin-opening))
                         (vegeta--eca--open s chat-id from-buffer))))))
        (add-hook 'eca-after-initialize-hook fn)
        (call-interactively #'eca)))
     ;; No ECA -- fall back to a rendered transcript.
     (t (vegeta--pop-to (vegeta--eca-open-transcript entry))))))

;;; Provider: :delete

(defun vegeta--eca-delete (entry)
  "Delete ENTRY's ECA chat through the running ECA server.
ECA owns its chat database, so deletion must go through the server's
`chat/delete' request: removing the `.transit.json' behind its back
leaves the index stale -- the row survives and can no longer be opened.
Signals an error (reported by `vegeta-execute') when ECA isn't running,
since there is no offline way to keep the index consistent."
  (let* ((ex (plist-get entry :extras))
         (chat-id (plist-get ex :chat-id))
         (session (and (require 'eca nil t)
                       (fboundp 'eca-session)
                       (ignore-errors (eca-session)))))
    (unless (and session
                 (eq (ignore-errors (eca--session-status session)) 'started)
                 (fboundp 'eca-api-request-sync))
      (error "ECA must be running to delete chat %s" chat-id))
    (eca-api-request-sync session
                          :method "chat/delete"
                          :params (list :chatId chat-id))
    ;; The server prunes its own cache; drop the file too in case a stale
    ;; copy is left behind.
    (let ((path (plist-get entry :id)))
      (when (and (stringp path) (file-exists-p path))
        (ignore-errors (delete-file path))))))

;;; Registration

(vegeta-register-provider
 (list :id 'eca
       :name "ECA"
       :list #'vegeta--eca-list
       :parse #'vegeta--eca-parse
       :visit #'vegeta--eca-visit
       :date-key #'vegeta--eca-date-key
       :delete #'vegeta--eca-delete
       ;; `:list' shells out to the local `eca' binary and reads the
       ;; local cache, so the provider is local-only.
       :local-only t
       ;; Keep the model level: ECA chats span providers/models
       ;; (deepseek/..., anthropic/...), so grouping by model is useful.
       :open-transcript #'vegeta--eca-open-transcript))

(provide 'vegeta-eca)
;;; vegeta-eca.el ends here
