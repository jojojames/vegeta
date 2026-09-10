;;; vegeta-codex-cli.el --- Codex CLI provider for vegeta -*- lexical-binding: t; coding: utf-8; -*-

;; Keywords: codex, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Lists native Codex rollouts from CODEX_HOME/sessions/YYYY/MM/DD and
;; resumes them in ghostel with `codex resume SESSION_ID'.  Metadata
;; reads are bounded and do not require the Codex executable or agent-shell.
;; The selected Codex home is preserved when a session is resumed.
;; Archived sessions are not listed.  Codex manages deletion and archival,
;; including its session indexes, so this provider does not delete rollouts.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'vegeta-core)

(defcustom vegeta-codex-cli-command "codex"
  "Executable used to resume Codex CLI sessions."
  :type 'string
  :group 'vegeta)

(defcustom vegeta-codex-home
  (file-name-as-directory
   (expand-file-name
    (let ((home (getenv "CODEX_HOME")))
      (if (and home (not (string-empty-p home))) home "~/.codex/"))))
  "Local Codex data directory containing the sessions subdirectory."
  :type 'directory
  :group 'vegeta)

;;; Discovery

(defun vegeta--codex-regular-file-p (attributes)
  "Return non-nil when ATTRIBUTES describe a regular file."
  (and attributes
       (null (file-attribute-type attributes))
       (eq (aref (file-attribute-modes attributes) 0) ?-)))

(defun vegeta--codex-list ()
  "Discover regular Codex rollouts without following directory symlinks."
  (let ((base (expand-file-name "sessions/" vegeta-codex-home)) entries)
    (unless (file-remote-p base)
      (let ((pending (list (cons base 0))))
        (while pending
          (pcase-let ((`(,directory . ,depth) (pop pending)))
            (unless (file-symlink-p (directory-file-name directory))
              (dolist (pair (condition-case nil
                                (directory-files-and-attributes directory t "\\`[^.]" t)
                              (file-error nil)))
                (let* ((file (car pair))
                       (attrs (cdr pair))
                       (name (file-name-nondirectory file)))
                  (cond
                   ((and (< depth 3) (eq t (file-attribute-type attrs))
                         (string-match-p
                          (if (= depth 0)
                              "\\`[0-9]\\{4\\}\\'"
                            "\\`[0-9]\\{2\\}\\'")
                          name))
                    (push (cons file (1+ depth)) pending))
                   ((and (vegeta--codex-regular-file-p attrs)
                         (string-match-p "\\`rollout-.*\\.jsonl\\'" name))
                    (push (list :provider 'codex-cli :id file :agent "Codex"
                                :mtime (float-time
                                        (file-attribute-modification-time attrs))
                                :extras (list :codex-home
                                              (expand-file-name vegeta-codex-home)))
                          entries))))))))))
    entries))

;;; Metadata

(defun vegeta--codex-read-jsonl (file limit)
  "Read complete JSON objects from at most LIMIT bytes of regular FILE.
Malformed records are skipped.  File and read-hook errors propagate."
  (let ((before (file-attributes file)))
    (unless (vegeta--codex-regular-file-p before)
      (user-error "Not a regular transcript file: %s" file))
    (with-temp-buffer
      (insert-file-contents file nil 0 limit)
      (let ((after (file-attributes file)) objects)
        (unless (vegeta--codex-regular-file-p after)
          (user-error "Transcript changed file type while reading: %s" file))
        (when (or (> (file-attribute-size before) limit)
                  (> (file-attribute-size after) limit))
          (goto-char (point-max))
          (unless (bolp) (delete-region (line-beginning-position) (point-max))))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((object (condition-case nil
                            (json-parse-string
                             (buffer-substring-no-properties (point) (line-end-position))
                             :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil)
                          (json-error nil))))
            (when (and (consp object) (consp (car object)))
              (push object objects)))
          (forward-line 1))
        (nreverse objects)))))

(defun vegeta--codex-user-text (content)
  "Extract a user prompt from CONTENT, excluding leading injected context.
Retain user text after a complete context wrapper and markup inside a prompt."
  (seq-some
   (lambda (block)
     (let ((text (and (proper-list-p block)
                      (equal (alist-get 'type block) "input_text")
                      (alist-get 'text block))))
       (when (stringp text)
         (setq text (string-trim text))
         (let ((start 0)
               (context-tag
                "<\\(environment_context\\|INSTRUCTIONS\\|permissions\\|skills_instructions\\|user_instructions\\|recommended_plugins\\)\\(?:[[:space:]][^>]*\\)?>"))
           ;; Consume leading wrappers without copying their remaining suffix
           ;; on every iteration.  An incomplete context block has no prompt.
           (while (and (< start (length text))
                       (string-match context-tag text start)
                       (= (match-beginning 0) start))
             (let ((closing (concat "</" (match-string 1 text) ">"))
                   (body-start (match-end 0)))
               (setq start
                     (if (string-match (regexp-quote closing) text body-start)
                         (or (string-match "[^ \t\r\n]" text (match-end 0))
                             (length text))
                       (length text)))))
           (when (> start 0) (setq text (substring text start))))
         (unless (or (string-empty-p text)
                     (string-match-p "\\`# AGENTS\\.md instructions" text))
           text))))
   (and (listp content) content)))

(defun vegeta--codex-parse (entry)
  "Read at most 262144 bytes of native Codex rollout metadata for ENTRY.
Use the thread id in session_meta, never a prompt's claimed session id.
Choose the first user prompt in file order across both native record forms."
  (condition-case err
      (let (session cwd timestamp model preview)
        (dolist (object (vegeta--codex-read-jsonl (plist-get entry :id) 262144))
          (let ((payload (alist-get 'payload object)))
            (when (listp payload)
              (pcase (alist-get 'type object)
                ("session_meta"
                 ;; Forked rollouts can contain older metadata after their own.
                 (unless session
                   (let ((id (alist-get 'id payload)))
                     (when (and (stringp id) (not (string-empty-p id)))
                       (setq session id cwd (alist-get 'cwd payload)
                             timestamp (alist-get 'timestamp payload))))))
                ("turn_context"
                 (unless model
                   (when (stringp (alist-get 'model payload))
                     (setq model (alist-get 'model payload)))))
                ("event_msg"
                 (when (and (null preview)
                            (equal (alist-get 'type payload) "user_message")
                            (stringp (alist-get 'message payload)))
                   (setq preview
                         (vegeta--codex-user-text
                          (list (list (cons 'type "input_text")
                                      (cons 'text (alist-get 'message payload))))))))
                ("response_item"
                 (when (and (null preview)
                            (equal (alist-get 'type payload) "message")
                            (equal (alist-get 'role payload) "user"))
                   (setq preview
                         (vegeta--codex-user-text (alist-get 'content payload)))))))))
        (unless (and (stringp session)
                     (string-match-p
                      "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
                      session))
          (user-error "No valid Codex thread id in rollout prefix"))
        (list :agent "Codex" :session-id session :cwd (and (stringp cwd) cwd)
              :started-at (and (stringp timestamp) timestamp) :model model
              :updated-at nil :ai-title nil :renamed nil
              :first-prompt (and preview (car (split-string (string-trim preview) "\n" t)))))
    (file-error (list :error (error-message-string err)))
    (user-error (list :error (error-message-string err)))))

;;; Visit

(defun vegeta--codex-visit (entry)
  "Resume Codex ENTRY in a terminal using its recorded data directory."
  (let* ((meta (vegeta--ensure-parsed entry))
         (session (plist-get meta :session-id))
         (cwd (plist-get meta :cwd))
         (home (or (plist-get (plist-get entry :extras) :codex-home)
                   vegeta-codex-home)))
    (unless (and (stringp session)
                 (string-match-p
                  "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
                  session))
      (user-error "No valid Codex thread id in rollout prefix"))
    (unless (and (stringp cwd) (not (string-empty-p cwd))
                 (file-name-absolute-p cwd) (not (file-remote-p cwd))
                 (file-directory-p cwd))
      (user-error "Codex session directory is unavailable: %s" cwd))
    (unless (and (stringp home) (not (string-empty-p home))
                 (not (file-remote-p home)) (file-directory-p home))
      (user-error "Codex home must be an existing local directory"))
    (unless (executable-find vegeta-codex-cli-command)
      (user-error "Codex executable not found: %s" vegeta-codex-cli-command))
    ;; Load ghostel on demand, as its autoloads need not define ghostel-exec.
    (unless (or vegeta-terminal-function (fboundp 'ghostel-exec)
                (and (require 'ghostel nil t) (fboundp 'ghostel-exec)))
      (user-error "Codex CLI requires ghostel or vegeta-terminal-function"))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "CODEX_HOME" (expand-file-name home))
      (vegeta--launch-terminal
       (format "*codex-resume: %s*" (substring session 0 8))
       vegeta-codex-cli-command (list "resume" session) cwd))))

(defun vegeta--codex-delete (_entry)
  "Reject deletion of a Codex rollout without updating Codex's indexes."
  (user-error "Manage Codex session deletion or archival in Codex"))

;;; Registration

(vegeta-register-provider
 (list :id 'codex-cli
       :name "Codex CLI"
       :list #'vegeta--codex-list
       :parse #'vegeta--codex-parse
       :visit #'vegeta--codex-visit
       :delete #'vegeta--codex-delete
       :skip-levels '(model)))

(provide 'vegeta-codex-cli)
;;; vegeta-codex-cli.el ends here
