;;; vegeta-claude-cli.el --- Claude CLI provider for vegeta -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Lists and resumes Claude CLI chat sessions stored under
;; `vegeta-claude-cli-projects-dir' (usually `~/.claude/projects/').
;; Each project directory contains one JSONL file per session; the
;; UUID in the filename is what `claude --resume' expects.
;;
;; Also exposes helpers for cross-referencing session JSONLs by cwd
;; and first-message timestamp, which the agent-shell provider uses
;; to recover a resumable session id for markdown transcripts whose
;; `**Session ID:**' header field is missing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'vegeta-core)

;;; Customization

(defcustom vegeta-claude-cli-command "claude"
  "Executable name for the Claude CLI."
  :type 'string
  :group 'vegeta)

(defcustom vegeta-claude-cli-resume-args
  (list "--resume")
  "Args prepended before the session UUID when resuming a Claude CLI chat.
The UUID is appended as the final argument."
  :type '(repeat string)
  :group 'vegeta)

(defcustom vegeta-claude-cli-projects-dir
  (expand-file-name "~/.claude/projects/")
  "Directory where Claude CLI stores per-project session JSONLs."
  :type 'directory
  :group 'vegeta)

;;; Project-dir encoding

(defun vegeta--claude-encode-path (path)
  "Encode PATH the way Claude CLI encodes project cwd into a directory name."
  (replace-regexp-in-string
   "[/.]" "-"
   (directory-file-name (expand-file-name path))))

(defvar vegeta--claude-project-decode-cache nil
  "Alist ((ENCODED . DECODED) ...) built at list-time.")

(defun vegeta--claude-decode-project-dir (encoded)
  "Return the actual project root for a Claude project ENCODED name.
Falls back to a trivial decoding when no known root matches."
  (or (cdr (assoc encoded vegeta--claude-project-decode-cache))
      (let ((decoded
             (concat "/"
                     (replace-regexp-in-string
                      "^-" "" encoded))))
        (setq decoded (replace-regexp-in-string "-" "/" decoded))
        decoded)))

(defun vegeta--claude-rebuild-decode-cache ()
  "Rebuild `vegeta--claude-project-decode-cache' from known roots."
  (setq vegeta--claude-project-decode-cache
        (mapcar (lambda (r)
                  (cons (vegeta--claude-encode-path r) r))
                (vegeta--project-roots))))

;;; JSONL cross-reference (used by agent-shell provider for session-id recovery)

(defvar vegeta--claude-sessions-by-cwd-cache
  (make-hash-table :test 'equal)
  "Cache of cwd -> list of (uuid . first-timestamp-seconds).")

(defun vegeta--claude-jsonl-first-timestamp (file)
  "Return the first `timestamp' field found in JSONL FILE, or nil."
  (with-temp-buffer
    (condition-case _
        (insert-file-contents file nil 0 4096)
      (error nil))
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          ts)
      (while (and (not ts) (not (eobp)))
        (let* ((line (buffer-substring-no-properties
                      (point) (line-end-position)))
               (obj (condition-case _
                        (json-read-from-string line)
                      (error nil))))
          (when-let* ((t2 (and obj (alist-get 'timestamp obj))))
            (setq ts t2)))
        (forward-line 1))
      ts)))

(defun vegeta--claude-sessions-for-cwd (cwd)
  "Return list of (UUID . SECONDS) for Claude CLI sessions in CWD."
  (let ((cached (gethash cwd vegeta--claude-sessions-by-cwd-cache
                         'not-cached)))
    (if (not (eq cached 'not-cached))
        cached
      (let* ((encoded (vegeta--claude-encode-path cwd))
             (dir (expand-file-name
                   encoded vegeta-claude-cli-projects-dir))
             sessions)
        (when (file-directory-p dir)
          (dolist (f (ignore-errors
                       (directory-files dir t "\\.jsonl\\'" t)))
            (when-let* ((ts (vegeta--claude-jsonl-first-timestamp f))
                        (secs (vegeta--iso-to-seconds ts)))
              (push (cons (file-name-base f) secs) sessions))))
        (puthash cwd sessions
                 vegeta--claude-sessions-by-cwd-cache)
        sessions))))

;;; Provider: :list

(defun vegeta--claude-list ()
  "List all Claude CLI session entries from ~/.claude/projects/*/*.jsonl."
  (when (file-directory-p vegeta-claude-cli-projects-dir)
    (vegeta--claude-rebuild-decode-cache)
    (let (entries)
      (dolist (proj-dir (ignore-errors
                          (directory-files
                           vegeta-claude-cli-projects-dir
                           t "\\`[^.]" t)))
        (when (file-directory-p proj-dir)
          (let ((encoded (file-name-nondirectory
                          (directory-file-name proj-dir))))
            (dolist (path (ignore-errors
                            (directory-files
                             proj-dir t "\\.jsonl\\'" t)))
              (let* ((attrs (file-attributes path))
                     (mtime (and attrs
                                 (float-time
                                  (file-attribute-modification-time attrs))))
                     (uuid (file-name-base path))
                     (repo (vegeta--claude-decode-project-dir encoded)))
                (push (list :provider 'claude-cli
                            :id path
                            :repo (file-name-as-directory repo)
                            :agent "Claude"
                            :mtime mtime
                            :extras (list :session-id uuid
                                          :encoded-dir encoded))
                      entries))))))
      entries)))

;;; Provider: :parse

(defun vegeta--claude-parse (entry)
  "Parse the first non-meta user message from a Claude CLI JSONL ENTRY."
  (let ((file (plist-get entry :id))
        (session-id (plist-get (plist-get entry :extras) :session-id))
        (bytes-to-read 65536)
        cwd model timestamp preview)
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 bytes-to-read)
        (error nil))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (while (and (not (eobp))
                    (or (null preview) (null cwd) (null timestamp)))
          (let ((line-end (line-end-position))
                obj)
            (setq obj
                  (condition-case _
                      (json-read-from-string
                       (buffer-substring-no-properties (point) line-end))
                    (error nil)))
            (when obj
              (unless timestamp
                (when-let* ((ts (alist-get 'timestamp obj)))
                  (setq timestamp ts)))
              (unless cwd
                (when-let* ((c (alist-get 'cwd obj)))
                  (setq cwd c)))
              (unless model
                (when-let* ((msg (alist-get 'message obj))
                            (m (alist-get 'model msg)))
                  (setq model m)))
              (unless preview
                (when (and (equal (alist-get 'type obj) "user")
                           (not (eq (alist-get 'isMeta obj) t))
                           (not (alist-get 'isMeta obj)))
                  (when-let* ((msg (alist-get 'message obj))
                              (content (alist-get 'content msg))
                              (text (cond
                                     ((stringp content) content)
                                     ((and (listp content) content)
                                      (or (alist-get 'text (car content))
                                          ""))
                                     (t ""))))
                    (let ((trimmed (string-trim text)))
                      (unless (or (string-empty-p trimmed)
                                  (string-prefix-p "<local-command-caveat>"
                                                   trimmed)
                                  (string-prefix-p "<command-name>"
                                                   trimmed)
                                  (string-prefix-p "<command-message>"
                                                   trimmed)
                                  (string-prefix-p "<command-args>"
                                                   trimmed)
                                  (string-prefix-p "<local-command-stdout>"
                                                   trimmed)
                                  (string-prefix-p "<local-command-stderr>"
                                                   trimmed))
                        (setq preview
                              (car (split-string trimmed "\n" t))))))))
            (forward-line 1)))))
    (list :agent "Claude"
          :model model
          :timestamp timestamp
          :cwd cwd
          :session-id session-id
          :preview preview))))

;;; Provider: :visit

(defun vegeta--claude-visit (entry)
  "Resume a Claude CLI ENTRY in a terminal via `claude --resume <uuid>'."
  (let* ((meta (vegeta--ensure-parsed entry))
         (session-id (or (plist-get meta :session-id)
                         (plist-get (plist-get entry :extras) :session-id)))
         (cwd (or (plist-get meta :cwd)
                  (plist-get entry :repo)
                  default-directory))
         (args (append vegeta-claude-cli-resume-args
                       (list session-id)))
         (name (format "*claude-resume: %s*" (substring session-id 0 8))))
    (message "vegeta[claude-cli]: %s %s (cwd=%s)"
             vegeta-claude-cli-command
             (string-join args " ") cwd)
    (vegeta--launch-terminal
     name vegeta-claude-cli-command args cwd)))

;;; Registration

(vegeta-register-provider
 (list :id 'claude-cli
       :name "Claude CLI"
       :list #'vegeta--claude-list
       :parse #'vegeta--claude-parse
       :visit #'vegeta--claude-visit))

(provide 'vegeta-claude-cli)
;;; vegeta-claude-cli.el ends here
