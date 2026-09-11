;;; vegeta-antigravity-cli.el --- Google Antigravity CLI (agy) provider -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, antigravity, gemini, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Lists and resumes conversations from Google's Antigravity CLI (`agy').
;; agy stores per-conversation data at:
;;
;;   ~/.gemini/antigravity-cli/brain/<UUID>/
;;     .system_generated/logs/transcript.jsonl   - line-per-step JSONL
;;     .system_generated/logs/transcript_full.jsonl
;;     .user_uploaded/                            - file uploads
;;     scratch/                                   - scratch files
;;
;;   ~/.gemini/antigravity-cli/conversations/<UUID>.db  - SQLite state
;;
;; A conversation's workspace URI is embedded in the `.db' file's
;; `trajectory_metadata_blob' column.  We extract it by scanning the
;; raw sqlite bytes for `file://...' — SQLite stores blobs inline in
;; the page data and the URI comes through unchanged, so no sqlite
;; library dependency is needed.
;;
;; RET on a row spawns a terminal running `agy --conversation <UUID>'
;; at the conversation's workspace directory.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(require 'vegeta-core)

;;; Customization

(defcustom vegeta-antigravity-cli-brain-dir
  (expand-file-name "~/.gemini/antigravity-cli/brain/")
  "Directory where agy stores per-conversation brain state.
One subdirectory per conversation, named by UUID."
  :type 'directory
  :group 'vegeta)

(defcustom vegeta-antigravity-cli-conversations-dir
  (expand-file-name "~/.gemini/antigravity-cli/conversations/")
  "Directory where agy stores per-conversation SQLite state.
One `<UUID>.db' per conversation."
  :type 'directory
  :group 'vegeta)

(defcustom vegeta-antigravity-cli-command "agy"
  "Executable name for the Antigravity CLI."
  :type 'string
  :group 'vegeta)

(defcustom vegeta-antigravity-cli-resume-args
  (list "--conversation")
  "Args prepended before the conversation UUID when resuming.
The UUID is appended as the final argument."
  :type '(repeat string)
  :group 'vegeta)

;;; Helpers

(defconst vegeta--agy-uuid-regexp
  "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
  "Match a canonical lowercase UUID.")

(defun vegeta--agy-cwd-from-db (db-file)
  "Return the workspace path for DB-FILE, or nil.
Scans the raw sqlite bytes for `file://' URIs (embedded in the
protobuf-encoded `trajectory_metadata_blob') and picks the first
that decodes to an actual directory on disk — this filters out
template placeholder URIs like `file:///absolute/path/to/file'
that appear in system-prompt boilerplate."
  (when (and db-file (file-readable-p db-file))
    (condition-case _
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally db-file)
          (goto-char (point-min))
          (let (found)
            (while (and (not found)
                        (re-search-forward
                         "file:///[[:alnum:]/._~%-]\\{3,200\\}" nil t))
              (let* ((uri (match-string 0))
                     (path (ignore-errors
                             (url-unhex-string (substring uri 7)))))
                (when (and path (file-directory-p path))
                  (setq found (file-name-as-directory
                               (expand-file-name path))))))
            found))
      (error nil))))

(defun vegeta--agy-transcript-path (uuid)
  "Return the transcript.jsonl path for conversation UUID."
  (expand-file-name
   (format "%s/.system_generated/logs/transcript.jsonl" uuid)
   vegeta-antigravity-cli-brain-dir))

(defun vegeta--agy-db-path (uuid)
  "Return the conversation SQLite path for conversation UUID."
  (expand-file-name (concat uuid ".db")
                    vegeta-antigravity-cli-conversations-dir))

;;; Provider: :list

(defun vegeta--agy-list ()
  "List all agy conversations under `vegeta-antigravity-cli-brain-dir'."
  (when (file-directory-p vegeta-antigravity-cli-brain-dir)
    (let (entries)
      (dolist (uuid (ignore-errors
                      (directory-files
                       vegeta-antigravity-cli-brain-dir nil "\\`[^.]" t)))
        (when (and (string-match-p vegeta--agy-uuid-regexp uuid)
                   (file-directory-p
                    (expand-file-name uuid
                                      vegeta-antigravity-cli-brain-dir)))
          (let* ((tx (vegeta--agy-transcript-path uuid))
                 (db (vegeta--agy-db-path uuid))
                 (cwd (vegeta--agy-cwd-from-db db))
                 (mtime (and (file-exists-p tx)
                             (float-time
                              (file-attribute-modification-time
                               (file-attributes tx))))))
            (when (file-exists-p tx)
              (push (list :provider 'antigravity-cli
                          :id tx
                          :repo cwd
                          :agent "Gemini"
                          :mtime mtime
                          :extras (list :uuid uuid
                                        :db db
                                        :cwd cwd))
                    entries)))))
      entries)))

;;; Provider: :parse

(defun vegeta--agy-strip-user-request (content)
  "Return the payload inside a `<USER_REQUEST>' block of CONTENT."
  (when (and content
             (string-match
              "<USER_REQUEST>[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*</USER_REQUEST>"
              content))
    (let ((raw (match-string 1 content)))
      (car (split-string raw "\n" t "[ \t]+")))))

(defun vegeta--agy-parse (entry)
  "Parse ENTRY's transcript.jsonl for started-at + first user prompt."
  (let ((file (plist-get entry :id))
        (uuid (plist-get (plist-get entry :extras) :uuid))
        (cwd (plist-get (plist-get entry :extras) :cwd))
        started prompt)
    (with-temp-buffer
      (condition-case _
          (insert-file-contents file nil 0 32768)
        (error nil))
      (goto-char (point-min))
      (let* ((line (buffer-substring-no-properties
                    (point) (line-end-position)))
             (json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (obj (condition-case _
                      (json-read-from-string line)
                    (error nil))))
        (when obj
          (setq started (alist-get 'created_at obj))
          (setq prompt (vegeta--agy-strip-user-request
                        (alist-get 'content obj))))))
    (list :agent "Gemini"
          :model nil
          :started-at started
          :updated-at nil
          :cwd cwd
          :session-id uuid
          :first-prompt prompt
          :ai-title nil
          :renamed nil)))

;;; Provider: :date-key

(defun vegeta--agy-date-key (entry)
  "Return YYYY-MM-DD for ENTRY.
Prefers the transcript's own `created_at' when cached, falls back
to the file mtime — both are set at list time and stable across
async parse chunks."
  (vegeta--default-date-key entry))

;;; Provider: :visit

(defun vegeta--agy-visit (entry)
  "Resume the agy conversation for ENTRY in a terminal.
Runs `agy --conversation <UUID>' at the recorded workspace."
  (let* ((uuid (plist-get (plist-get entry :extras) :uuid))
         (cwd (or (plist-get entry :repo)
                  (plist-get (plist-get entry :extras) :cwd)
                  default-directory))
         (args (append vegeta-antigravity-cli-resume-args (list uuid)))
         (name (format "*agy-resume: %s*" (substring uuid 0 8))))
    (message "vegeta[antigravity-cli]: %s %s (cwd=%s)"
             vegeta-antigravity-cli-command
             (string-join args " ") cwd)
    (vegeta--launch-terminal
     name vegeta-antigravity-cli-command args cwd)))

;;; Provider: :delete

(defun vegeta--agy-delete (entry)
  "Delete an agy conversation's brain dir and SQLite db.
Removes:
  ~/.gemini/antigravity-cli/brain/<UUID>/     (recursive)
  ~/.gemini/antigravity-cli/conversations/<UUID>.db"
  (let* ((uuid (plist-get (plist-get entry :extras) :uuid))
         (brain (and uuid
                     (expand-file-name uuid
                                       vegeta-antigravity-cli-brain-dir)))
         (db (plist-get (plist-get entry :extras) :db)))
    (when (and brain (file-directory-p brain))
      (delete-directory brain t))
    (when (and db (file-exists-p db))
      (delete-file db))))

;;; Registration

;;; Search

(defcustom vegeta-antigravity-cli-search-python (executable-find "python3")
  "Path to `python3' used by the Antigravity CLI search helper.
Only this value's basename is used on a remote host."
  :type '(choice (file :tag "python3 executable") (string :tag "Command") (const nil))
  :group 'vegeta)

(defconst vegeta--agy-search-py
  "import sys, os, glob, json

try:
    sys.stdout.reconfigure(line_buffering=True)
except Exception:
    pass

scope = sys.argv[1] if len(sys.argv) > 1 else 'content'
targets = sys.argv[2:]

want = {
    'user': ('user',),
    'agent': ('agent',),
    'content': ('user', 'agent'),
    'thoughts': ('user', 'agent'),
    'all': ('user', 'agent', 'other'),
}.get(scope, ('user', 'agent'))

def kind_of(src):
    if not isinstance(src, str):
        return 'other'
    if src.startswith('USER'):
        return 'user'
    if src == 'MODEL':
        return 'agent'
    return 'other'

def clean(c):
    if not isinstance(c, str):
        return []
    a = c.find('<USER_REQUEST>')
    b = c.find('</USER_REQUEST>')
    if a != -1 and b != -1 and b > a:
        c = c[a + 14:b]
    else:
        for tag in ('<ADDITIONAL_METADATA>', '<USER_SETTINGS_CHANGE>'):
            i = c.find(tag)
            if i != -1:
                c = c[:i]
    return c.splitlines()

def files_for(t):
    if os.path.isfile(t):
        return [t]
    if not os.path.isdir(t):
        return []
    out = []
    for base, dirs, names in os.walk(t):
        if 'chunks' in base.split(os.sep):
            continue
        for n in names:
            if n.endswith('.jsonl') and n != 'transcript_full.jsonl':
                out.append(os.path.join(base, n))
    return out

for t in targets:
    for path in sorted(files_for(t)):
        try:
            fh = open(path, 'r', errors='ignore')
        except OSError:
            continue
        try:
            for i, line in enumerate(fh, 1):
                line = line.rstrip()
                if not line:
                    continue
                try:
                    obj = json.loads(line)
                except Exception:
                    continue
                k = kind_of(obj.get('source'))
                if k not in want:
                    continue
                for ln in clean(obj.get('content')):
                    ln = ln.rstrip()
                    if ln.strip() and any(ch.isalnum() for ch in ln):
                        print('%s:%d:%s' % (path, i, ln))
        finally:
            fh.close()
"
  "Python helper that streams searchable Antigravity CLI transcript lines.
Each line of `transcript.jsonl' carries a `source' (USER_* or MODEL) and
a `content' string; the helper unwraps `<USER_REQUEST>' payloads, drops
metadata blocks, and prints FILE:LINE:TEXT.  SCOPE is argv[1]; the rest
are TARGETS (the brain dir, or specific `.jsonl' files).  Inlined by
`vegeta--agy-search-command' so it runs locally and over ssh remotely.")

(defun vegeta--agy-search-python-for (host)
  "Return the python3 command to use when searching HOST."
  (let ((python (or vegeta-antigravity-cli-search-python "python3")))
    (if (vegeta--local-host-p host)
        python
      (file-name-nondirectory python))))

(defun vegeta--agy--search-roots (host)
  "Return native (HOST-side) roots to search for Antigravity CLI chats."
  (if (vegeta--local-host-p host)
      (list vegeta-antigravity-cli-brain-dir)
    (list (vegeta--host-join host "~/.gemini/antigravity-cli/brain/"))))

(defun vegeta--agy-search-command (host scope targets)
  "Return a shell command that streams Antigravity CLI matches on HOST.
TARGETS, when non-nil, restricts the search to those native paths (the
marked conversations or transcript files); nil falls back to the brain
dir.  SCOPE selects user/agent content.  The Python helper is inlined so
it runs locally and, ssh-wrapped by `fzfa-tramp', on a remote host."
  (let ((paths (or targets (vegeta--agy--search-roots host)))
        (python (vegeta--agy-search-python-for host)))
    (when (and python paths)
      (format "%s -c %s %s%s"
              (shell-quote-argument python)
              (shell-quote-argument vegeta--agy-search-py)
              (shell-quote-argument (format "%s" (or scope 'content)))
              (concat " " (mapconcat #'shell-quote-argument paths " "))))))

(vegeta-register-provider
 (list :id 'antigravity-cli
       :name "Antigravity CLI"
       :list #'vegeta--agy-list
       :parse #'vegeta--agy-parse
       :visit #'vegeta--agy-visit
       :date-key #'vegeta--agy-date-key
       :delete #'vegeta--agy-delete
       ;; Data roots, in `~'-relative form, for remote (`vegeta-hosts')
       ;; listing over Tramp.
       :host-dirs '((vegeta-antigravity-cli-brain-dir
                     . "~/.gemini/antigravity-cli/brain/")
                    (vegeta-antigravity-cli-conversations-dir
                     . "~/.gemini/antigravity-cli/conversations/"))
       :search-command #'vegeta--agy-search-command
       ;; Every agy entry is Gemini; skip the redundant model level.
       :skip-levels '(model)))

(provide 'vegeta-antigravity-cli)
;;; vegeta-antigravity-cli.el ends here
