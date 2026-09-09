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

(require 'agent-shell)

(require 'vegeta-core)
(require 'vegeta-claude-cli)

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
        (goto-char (point-min))
        (when (re-search-forward "^## User[^\n]*$" nil t)
          (forward-line 1)
          (while (and (not (eobp))
                      (looking-at-p "^[ \t]*$"))
            (forward-line 1))
          (unless (eobp)
            (let* ((line (buffer-substring-no-properties
                          (point) (line-end-position)))
                   (stripped (if (string-match "\\`>[ \t]*\\(.*\\)\\'" line)
                                 (match-string 1 line)
                               line))
                   (trimmed (string-trim stripped)))
              (unless (string-empty-p trimmed)
                (setq preview trimmed)))))
        (list :agent agent
              :model model
              :timestamp started
              :cwd cwd
              :session-id session-id
              :preview preview)))))

;;; Cross-reference: transcript -> Claude CLI session-id

(defun vegeta--find-claude-session-for-transcript
    (agent-name cwd started-str)
  "Return a Claude CLI session UUID matching a transcript, or nil.
Matches when AGENT-NAME resolves to Claude, CWD has Claude sessions,
and one of them started within 60 seconds of STARTED-STR."
  (when (and agent-name cwd started-str
             (string-match-p "\\`Claude" agent-name))
    (when-let* ((tx-secs (vegeta--iso-to-seconds started-str))
                (sessions (vegeta--claude-sessions-for-cwd cwd)))
      (let (best-uuid best-delta)
        (dolist (s sessions)
          (let ((delta (abs (- tx-secs (cdr s)))))
            (when (or (null best-delta) (< delta best-delta))
              (setq best-uuid (car s)
                    best-delta delta))))
        (and best-uuid best-delta (< best-delta 60) best-uuid)))))

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
  "Visit an agent-shell ENTRY: resume live buffer, or start a new shell.
When the transcript lacks a `**Session ID:**' header, cross-reference
Claude CLI's own JSONL storage by cwd + start-timestamp to recover the
resumable session id.  As a last resort, hand off to agent-shell's own
session picker via `session-strategy'."
  (let* ((meta (vegeta--ensure-parsed entry))
         (agent-name (plist-get meta :agent))
         (cwd (plist-get meta :cwd))
         (started (plist-get meta :timestamp))
         (session-id (or (plist-get meta :session-id)
                         (vegeta--find-claude-session-for-transcript
                          agent-name cwd started)))
         (session-source (cond
                          ((plist-get meta :session-id) 'from-transcript)
                          (session-id 'from-claude-cli)
                          (t 'none)))
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
                          :prompt "Start with agent: ")))
             ;; When we don't know the session id, hand off to agent-shell's
             ;; own session picker (ACP `session/list' → `session/resume').
             ;; This is the same flow the user sees from `agent-shell-new-shell'.
             (session-strategy (if session-id 'new 'prompt)))
        (message "vegeta[agent-shell]: file=%s agent=%S session=%S (%s) config=%s (%s) strategy=%s cwd=%s"
                 (file-name-nondirectory (plist-get entry :id))
                 agent-name session-id session-source
                 (map-elt config :identifier) config-source
                 session-strategy default-directory)
        (let ((buf (agent-shell--start
                    :config config
                    :session-id session-id
                    :session-strategy session-strategy
                    :new-session t
                    :no-focus t)))
          (vegeta--pop-to buf)))))))

;;; Registration

(vegeta-register-provider
 (list :id 'agent-shell
       :name "agent-shell"
       :list #'vegeta--agent-shell-list
       :parse #'vegeta--agent-shell-parse
       :visit #'vegeta--agent-shell-visit))

(provide 'vegeta-agent-shell)
;;; vegeta-agent-shell.el ends here
