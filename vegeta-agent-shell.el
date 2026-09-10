;;; vegeta-agent-shell.el --- agent-shell provider for vegeta -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.60"))

;;; Commentary:
;;
;; Lists and resumes markdown transcripts written by agent-shell under
;; each project's `.agent-shell/transcripts/' directory.
;;
;; When a transcript header lacks a `**Session ID:**' field (older
;; agent-shell versions didn't populate it), we cross-reference the
;; Claude CLI JSONL storage by (cwd + start timestamp) to recover a
;; resumable session id.  As a last resort, we hand off to
;; agent-shell's own session picker via `session-strategy'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

;; agent-shell is loaded lazily inside `vegeta--agent-shell-visit'.
;; Keeping it out of the top-level requires means this file can be
;; loaded in a minimal Emacs (e.g. an `async' worker) that has no
;; agent-shell installed — the parser doesn't need it, only visit.
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--auto-preferred-config "agent-shell")
(declare-function agent-shell--resolved-agent-configs "agent-shell")
(defvar agent-shell--state)

(require 'vegeta-core)
(require 'vegeta-claude-cli)

(defcustom vegeta-agent-shell-orphan-prompt
  "Please re-read the transcript at %s and summarize what we were working on so I can continue."
  "Prompt inserted into a fresh shell when visiting an orphaned chat.
An orphaned agent-shell entry is one whose session is no longer
resumable (no session id in the transcript header and no matching
Claude CLI JSONL to cross-reference).  Rather than dropping the user
into an empty shell, we start a new one at the transcript's cwd and
pre-fill this prompt so the model can summarize prior work.  %s is
replaced with the transcript file path.  The user still has to press
RET to send."
  :type 'string
  :group 'vegeta)

;;; Discovery

(defun vegeta--agent-shell-transcripts-for-root (root)
  "Return transcript file paths under project ROOT."
  (let ((dir (expand-file-name ".agent-shell/transcripts/" root)))
    (when (file-directory-p dir)
      (ignore-errors
        (directory-files dir t "\\.md\\'" t)))))

(defun vegeta--agent-shell-list ()
  "List all agent-shell transcript entries across known projects."
  (let (entries)
    (dolist (root (vegeta--project-roots))
      (dolist (path (vegeta--agent-shell-transcripts-for-root root))
        (let* ((attrs (file-attributes path))
               (mtime (and attrs
                           (float-time
                            (file-attribute-modification-time attrs)))))
          (push (list :provider 'agent-shell
                      :id path
                      :repo root
                      :agent nil
                      :mtime mtime
                      :extras (list :file path))
                entries))))
    entries))

;;; Parse

(defun vegeta--agent-shell-parse (entry)
  "Parse the transcript header for ENTRY, returning a metadata plist."
  (let ((file (plist-get entry :id)))
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 8192)
        (error nil))
      (let (agent started session-id model cwd preview)
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Agent:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq agent (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Started:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq started (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Working Directory:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq cwd (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Session ID:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq session-id (string-trim (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*Model:\\*\\*[ \t]+\\(.*\\)$" nil t)
          (setq model (string-trim (match-string 1))))
        ;; Scan up to three User blocks looking for the first non-blank
        ;; content line.  Skip empty lines, bare `>' prompt markers, and
        ;; markdown code fences.  Strip a leading `> ' from any line
        ;; before considering it.
        (goto-char (point-min))
        (let ((tries 0))
          (while (and (null preview)
                      (< tries 3)
                      (re-search-forward "^## User[^\n]*$" nil t))
            (cl-incf tries)
            (let ((block-end (or (save-excursion
                                   (when (re-search-forward "^## " nil t)
                                     (match-beginning 0)))
                                 (point-max))))
              (forward-line 1)
              (while (and (null preview) (< (point) block-end))
                (let* ((line (buffer-substring-no-properties
                              (point) (line-end-position)))
                       (stripped (if (string-match
                                      "\\`>[ \t]*\\(.*\\)\\'" line)
                                     (match-string 1 line)
                                   line))
                       (trimmed (string-trim stripped)))
                  (unless (or (string-empty-p trimmed)
                              (string-prefix-p "```" trimmed))
                    (setq preview trimmed)))
                (forward-line 1)))))
        (let ((xref (and (not session-id)
                         (vegeta--find-claude-session-for-transcript
                          agent cwd started))))
          (list :agent agent
                :model model
                :started-at started
                :updated-at nil
                :cwd cwd
                :session-id (or session-id (plist-get xref :uuid))
                :first-prompt preview
                :ai-title (plist-get xref :ai-title)
                :renamed nil))))))

;;; Cross-reference: transcript -> Claude CLI session

(defun vegeta--find-claude-session-for-transcript
    (agent-name cwd started-str)
  "Return a plist for a Claude CLI session matching a transcript, or nil.
Result plist has keys `:uuid' and `:ai-title'.  Matches when AGENT-NAME
resolves to Claude, CWD has Claude sessions, and one of them started
within 60 seconds of STARTED-STR."
  (when (and agent-name cwd started-str
             (string-match-p "\\`Claude" agent-name))
    (when-let* ((tx-secs (vegeta--iso-to-seconds started-str))
                (sessions (vegeta--claude-sessions-for-cwd cwd)))
      (let (best-uuid best-headers best-delta)
        (dolist (s sessions)
          (let* ((first-ts (plist-get (cdr s) :first-ts))
                 (secs (and first-ts (vegeta--iso-to-seconds first-ts)))
                 (delta (and secs (abs (- tx-secs secs)))))
            (when (and delta (or (null best-delta) (< delta best-delta)))
              (setq best-uuid (car s)
                    best-headers (cdr s)
                    best-delta delta))))
        (when (and best-uuid best-delta (< best-delta 60))
          (list :uuid best-uuid
                :ai-title (plist-get best-headers :ai-title)))))))

;;; Visit

(defun vegeta--live-agent-shell-buffer-for-session (session-id)
  "Return a live agent-shell buffer whose session id equals SESSION-ID."
  (and session-id
       (seq-find (lambda (buf)
                   (with-current-buffer buf
                     (equal session-id
                            (map-nested-elt agent-shell--state
                                            '(:session :id)))))
                 (agent-shell-buffers))))

(defun vegeta--config-for-agent-name (name)
  "Return the agent-shell config whose display name matches NAME, or nil."
  (when (and name (fboundp 'agent-shell--resolved-agent-configs))
    (let ((configs (agent-shell--resolved-agent-configs))
          (name-fold (downcase name)))
      (or (seq-find (lambda (cfg)
                      (or (equal name (map-elt cfg :mode-line-name))
                          (equal name (map-elt cfg :buffer-name))))
                    configs)
          (seq-find (lambda (cfg)
                      (let ((mln (or (map-elt cfg :mode-line-name) ""))
                            (bn (or (map-elt cfg :buffer-name) "")))
                        (or (string-search name-fold (downcase mln))
                            (string-search name-fold (downcase bn))
                            (string-search (downcase mln) name-fold)
                            (string-search (downcase bn) name-fold))))
                    configs)))))

(defun vegeta--agent-shell-visit (entry)
  "Visit an agent-shell ENTRY.
On the local host the session is resumed (see
`vegeta--agent-shell--visit-local').  On a remote `vegeta-hosts'
machine an agent-shell session cannot be resumed, so the remote
transcript is opened read-only instead."
  (if (vegeta--local-host-p vegeta--current-host)
      (vegeta--agent-shell--visit-local entry)
    (vegeta--pop-to (find-file-noselect (plist-get entry :id)))))

(defun vegeta--agent-shell--visit-local (entry)
  "Resume or start an agent-shell session for ENTRY on the local host.
When `:session-id' is known (either from the transcript header or from
parse-time Claude CLI cross-reference), resume that session — or jump
to its live buffer if one is already open.

When the entry is orphaned (no resumable session), start a fresh shell
at the recorded cwd and prefill `vegeta-agent-shell-orphan-prompt' so
the model can be asked to re-read the transcript, rather than dropping
the user into an empty shell that has no idea what preceded it.  The
user still has to press RET to send the prefilled prompt.

Use \\[universal-argument] before RET on the row to bypass this entirely
and open the raw transcript file instead."
  (require 'agent-shell)
  (let* ((meta (vegeta--ensure-parsed entry))
         (agent-name (plist-get meta :agent))
         (cwd (plist-get meta :cwd))
         (session-id (plist-get meta :session-id))
         (live (vegeta--live-agent-shell-buffer-for-session session-id)))
    (cond
     (live
      (vegeta--pop-to live))
     (t
      (let* ((default-directory
              (if (and cwd (file-directory-p cwd))
                  (file-name-as-directory cwd)
                default-directory))
             (matched (vegeta--config-for-agent-name agent-name))
             (auto (and (not matched)
                        (fboundp 'agent-shell--auto-preferred-config)
                        (agent-shell--auto-preferred-config)))
             (config-source (cond (matched 'matched)
                                  (auto 'auto-preferred)
                                  (t 'prompt)))
             (config (or matched
                         auto
                         (agent-shell-select-config
                          :prompt "Start with agent: "))))
        (message "vegeta[agent-shell]: file=%s agent=%S session=%S config=%s (%s) mode=%s cwd=%s"
                 (file-name-nondirectory (plist-get entry :id))
                 agent-name session-id
                 (map-elt config :identifier) config-source
                 (if session-id "resume" "orphan+prompt")
                 default-directory)
        (let ((buf (agent-shell--start
                    :config config
                    :session-id session-id
                    :session-strategy 'new
                    :new-session t
                    :no-focus t)))
          (unless session-id
            (vegeta--agent-shell-prefill-orphan-prompt
             buf (plist-get entry :id)))
          (vegeta--pop-to buf)))))))

(defun vegeta--agent-shell-prefill-orphan-prompt (buffer transcript-path)
  "Insert `vegeta-agent-shell-orphan-prompt' into BUFFER via agent-shell.
Uses `agent-shell-insert', which is aware of shell-maker's read-only
regions and defers via the `prompt-ready' event when the shell is still
initializing.  Falls back to the kill-ring if the insertion signals
so the user can still paste the prompt manually."
  (when (buffer-live-p buffer)
    (let ((text (format vegeta-agent-shell-orphan-prompt transcript-path)))
      (condition-case err
          (with-current-buffer buffer
            (agent-shell-insert :text text :no-focus t
                                :shell-buffer buffer))
        (error
         (kill-new text)
         (message "vegeta: couldn't prefill (%s); prompt copied to kill-ring"
                  (error-message-string err)))))))

;;; Date key

(defun vegeta--agent-shell-date-key (entry)
  "Return YYYY-MM-DD extracted from an agent-shell transcript filename.
Transcript filenames follow the `YYYY-MM-DD-HH-MM-SS.md' convention,
so the date is available deterministically without parsing the file."
  (let ((name (file-name-nondirectory (or (plist-get entry :id) ""))))
    (if (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)-"
                      name)
        (match-string 1 name)
      (vegeta--default-date-key entry))))

;;; Registration

(vegeta-register-provider
 (list :id 'agent-shell
       :name "agent-shell"
       :list #'vegeta--agent-shell-list
       :parse #'vegeta--agent-shell-parse
       :visit #'vegeta--agent-shell-visit
       :date-key #'vegeta--agent-shell-date-key
       ;; Discovery is driven by `vegeta--project-roots', which resolves
       ;; to `vegeta-extra-project-roots' on a remote host — so a stray
       ;; directory (local or remote) can be targeted directly.  On a
       ;; remote host `:visit' opens the transcript instead of resuming.
       ))

(provide 'vegeta-agent-shell)
;;; vegeta-agent-shell.el ends here
