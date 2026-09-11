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
The agent runs on ENTRY's host (see `vegeta--agent-shell--visit-main'):
locally for localhost, or over Tramp for a remote `vegeta-hosts'
machine, where acp starts the agent process on that machine.  When a
remote resume is not possible — e.g. the agent executable is missing on
this machine, which agent-shell checks with a local `executable-find' —
the remote transcript is opened read-only instead."
  (condition-case err
      (vegeta--agent-shell--visit-main entry)
    (error
     (if (vegeta--local-host-p vegeta--current-host)
         (signal (car err) (cdr err))
       (message "vegeta[agent-shell]: %s; opening transcript"
                (error-message-string err))
       (vegeta--pop-to (find-file-noselect (plist-get entry :id)))))))

(defun vegeta--agent-shell--visit-main (entry)
  "Resume or start an agent-shell session for ENTRY on its host.
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

;;; Search

(defcustom vegeta-agent-shell-search-python (executable-find "python3")
  "Path to `python3' used by the agent-shell search helper.
The helper is sent inline as a `python3 -c' program, so `python3' must
exist on whichever host is searched (locally, or on a remote host's
PATH).  On a remote host only this value's basename is used, so the
host's own PATH resolves the interpreter; set it to a bare `\"python3\"'
if the remote names it differently."
  :type '(choice (file :tag "python3 executable") (string :tag "Command") (const nil))
  :group 'vegeta)

(defconst vegeta--agent-shell-search-py
  "import sys, os, glob

try:
    sys.stdout.reconfigure(line_buffering=True)
except Exception:
    pass

scope = sys.argv[1] if len(sys.argv) > 1 else 'content'
roots = sys.argv[2:]

want = {
    'user': ('user',),
    'agent': ('agent',),
    'content': ('user', 'agent'),
    'thoughts': ('user', 'agent', 'thoughts'),
    'all': ('user', 'agent', 'thoughts', 'other'),
}.get(scope, ('user', 'agent'))

def classify(s):
    if s.startswith('### '):
        return 'tool'
    if s.startswith('**Tool:**'):
        return 'tool'
    if s.startswith(\"## Agent's Thoughts\"):
        return 'thoughts'
    if s.startswith('## User '):
        return 'user'
    if s.startswith('## Agent '):
        return 'agent'
    if s.startswith('## '):
        return 'other'
    return None

def scan(path):
    kind = 'other'
    try:
        fh = open(path, 'r', errors='ignore')
    except OSError:
        return
    try:
        for i, line in enumerate(fh, 1):
            s = line.rstrip()
            c = classify(s)
            if c is not None:
                kind = c
                continue
            if kind == 'tool':
                continue
            if not s.strip():
                continue
            if s.strip() in ('---', '```', '~~~'):
                continue
            if kind in want:
                print('%s:%d:%s' % (path, i, s))
    finally:
        fh.close()

for root in roots:
    base = os.path.join(root, '.agent-shell', 'transcripts')
    for path in sorted(glob.glob(os.path.join(base, '*.md'))):
        scan(path)
"
  "Python helper that streams searchable agent-shell transcript lines.
It is structural: it tracks the current `## User' / `## Agent' /
`## Agent's Thoughts' section and any `### Tool Call' (or `**Tool:**')
block, then emits only the requested kinds.  SCOPE comes from argv[1]
\(user / agent / content = user+agent / thoughts / all), roots follow
as argv[2:].  Tool-call bodies, file paths, command output and compile
logs are therefore excluded for the default `content' scope.  Inlined
by `vegeta--agent-shell-search-command' so it runs unchanged locally
and, wrapped by `fzfa-tramp', over ssh on a remote host.")

(defun vegeta--agent-shell--search-roots (host)
  "Return native (HOST-side) project roots to search for agent-shell."
  (let ((vegeta--current-host host))
    (delq nil
          (mapcar (lambda (r) (vegeta--unhostify host r))
                  (vegeta--project-roots)))))

(defun vegeta--agent-shell-search-python-for (host)
  "Return the python3 command to use when searching HOST.
On the local host the resolved `vegeta-agent-shell-search-python' is
used.  On a remote host that local absolute path is meaningless, so
only its basename is used and the host's PATH resolves it."
  (let ((python (or vegeta-agent-shell-search-python "python3")))
    (if (vegeta--local-host-p host)
        python
      (file-name-nondirectory python))))

(defun vegeta--agent-shell-search-command (host scope)
  "Return a shell command that streams agent-shell matches on HOST.
SCOPE selects which transcript sections are emitted (`user', `agent',
`content', `thoughts' or `all').  The Python helper is inlined via
`python3 -c' (not a temp file) so it works locally and, ssh-wrapped by
`fzfa-tramp', on a remote host; roots are native host paths and the
interpreter is named so the host's PATH resolves it."
  (let ((roots (vegeta--agent-shell--search-roots host))
        (python (vegeta--agent-shell-search-python-for host)))
    (when python
      (format "%s -c %s %s%s"
              (shell-quote-argument python)
              (shell-quote-argument vegeta--agent-shell-search-py)
              (shell-quote-argument (format "%s" (or scope 'content)))
              (if roots
                  (concat " " (mapconcat #'shell-quote-argument roots " "))
                "")))))

;;; Registration

(vegeta-register-provider
 (list :id 'agent-shell
       :name "Agent Shell"
       :list #'vegeta--agent-shell-list
       :parse #'vegeta--agent-shell-parse
       :visit #'vegeta--agent-shell-visit
       :date-key #'vegeta--agent-shell-date-key
       ;; Discovery is driven by `vegeta--project-roots', which resolves
       ;; to `vegeta-extra-project-roots' on a remote host — so a stray
       ;; directory (local or remote) can be targeted directly.  On a
       ;; remote host `:visit' resumes the agent over Tramp (falling back
       ;; to the transcript when that isn't possible).
       ;;
       ;; The Claude projects dir is listed here too because `:parse'
       ;; cross-references Claude CLI JSONLs to recover a resumable
       ;; session id for orphaned transcripts — on a remote host that
       ;; must read the *remote* Claude dir.
       :host-dirs '((vegeta-claude-cli-projects-dir . "~/.claude/projects/"))
       ;; Owns its own full-text search script, sent to the host (local
       ;; or, via `fzfa-tramp', remote).  See `vegeta-search'.
       :search-command #'vegeta--agent-shell-search-command))

(provide 'vegeta-agent-shell)
;;; vegeta-agent-shell.el ends here
