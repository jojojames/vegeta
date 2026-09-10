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

(defcustom vegeta-claude-cli-session-env-dir
  (expand-file-name "~/.claude/session-env/")
  "Directory where Claude CLI stores per-session env sidecars.
Each session has a subdirectory named after its UUID.  Removed by the
provider's `:delete' hook alongside the main JSONL."
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
  "Cache of cwd -> list of (UUID . HEADERS-PLIST).
HEADERS-PLIST has keys `:first-ts' (string) and `:ai-title' (string or nil).")

(defun vegeta--claude-jsonl-headers (file)
  "Return a plist of header fields extracted from JSONL FILE.
Keys: `:first-ts' (first message timestamp) and `:ai-title' (session summary).
Nil values indicate the field wasn't present in the scanned prefix."
  (let (first-ts ai-title)
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 16384)
        (error nil))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (max-lines 30)
            (n 0))
        (while (and (not (eobp))
                    (< n max-lines)
                    (or (null first-ts) (null ai-title)))
          (let* ((line (buffer-substring-no-properties
                        (point) (line-end-position)))
                 (obj (condition-case _
                          (json-read-from-string line)
                        (error nil))))
            (when obj
              (unless first-ts
                (when-let* ((ts (alist-get 'timestamp obj)))
                  (setq first-ts ts)))
              (unless ai-title
                (when-let* ((t2 (alist-get 'aiTitle obj)))
                  (setq ai-title t2)))))
          (forward-line 1)
          (cl-incf n))))
    (list :first-ts first-ts :ai-title ai-title)))

(defun vegeta--claude-sessions-for-cwd (cwd)
  "Return list of (UUID . HEADERS-PLIST) for Claude CLI sessions in CWD.
See `vegeta--claude-jsonl-headers' for HEADERS-PLIST keys."
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
            (let ((headers (vegeta--claude-jsonl-headers f)))
              (when (plist-get headers :first-ts)
                (push (cons (file-name-base f) headers) sessions)))))
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
  "Parse metadata from a Claude CLI JSONL ENTRY.
Extracts the session cwd, model, first user prompt, first-message
timestamp, and the `aiTitle' summary Claude Code generates on
first response."
  (let ((file (plist-get entry :id))
        (session-id (plist-get (plist-get entry :extras) :session-id))
        (bytes-to-read 65536)
        cwd model timestamp preview ai-title)
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 bytes-to-read)
        (error nil))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (while (and (not (eobp))
                    (or (null preview) (null cwd) (null timestamp)
                        (null ai-title)))
          (let ((line-end (line-end-position))
                (line-start (point))
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
              (unless ai-title
                (when-let* ((t2 (alist-get 'aiTitle obj)))
                  (setq ai-title t2)))
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
                              (car (split-string trimmed "\n" t)))))))))
            ;; Always advance, even when the line failed to parse (a
            ;; truncated line at the read boundary yields `obj' = nil).
            ;; Break out if `forward-line' can't move — e.g. at eob or
            ;; a last line with no newline — so we never loop forever.
            (forward-line 1)
            (when (= (point) line-start)
              (goto-char (point-max)))))))
    (list :agent "Claude"
          :model model
          :started-at timestamp
          :updated-at nil
          :cwd cwd
          :session-id session-id
          :first-prompt preview
          :ai-title ai-title
          :renamed nil)))

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

;;; Delete

(defun vegeta--claude-delete (entry)
  "Remove ENTRY's Claude session storage.
Deletes both the main `.jsonl' file and its `session-env/<uuid>/'
sidecar directory (Claude Code writes per-session env state there).
The parent project directory under `vegeta-claude-cli-projects-dir'
is left alone even if it becomes empty, since Claude Code recreates
it on demand."
  (let* ((path (plist-get entry :id))
         (uuid (or (plist-get (plist-get entry :extras) :session-id)
                   (and path (file-name-base path))))
         (env-dir (and uuid
                       (expand-file-name
                        uuid vegeta-claude-cli-session-env-dir))))
    (when (and path (file-exists-p path))
      (delete-file path))
    (when (and env-dir (file-directory-p env-dir))
      (delete-directory env-dir t))))

;;; Date key

(defun vegeta--claude-date-key (entry)
  "Return YYYY-MM-DD for a Claude CLI ENTRY.
Reads from the cached per-cwd JSONL header (populated lazily by
`vegeta--claude-sessions-for-cwd') so the value is deterministic
before the full parse runs.  Falls back to mtime, then `unknown'."
  (let* ((repo (plist-get entry :repo))
         (uuid (plist-get (plist-get entry :extras) :session-id))
         (sessions (and repo uuid
                        (vegeta--claude-sessions-for-cwd repo)))
         (headers (and sessions (cdr (assoc uuid sessions))))
         (first-ts (plist-get headers :first-ts)))
    (cond
     ((and first-ts (string-match
                     "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
                     first-ts))
      (match-string 1 first-ts))
     (t (vegeta--default-date-key entry)))))

;;; Registration

(vegeta-register-provider
 (list :id 'claude-cli
       :name "Claude CLI"
       :list #'vegeta--claude-list
       :parse #'vegeta--claude-parse
       :visit #'vegeta--claude-visit
       :date-key #'vegeta--claude-date-key
       :delete #'vegeta--claude-delete
       ;; Every claude-cli entry is Claude; the model level would just
       ;; wrap everything in a single `Claude (N)' node — noise.
       :skip-levels '(model)))

(provide 'vegeta-claude-cli)
;;; vegeta-claude-cli.el ends here
