;;; vegeta-core.el --- Core infrastructure for vegeta -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (project "0.9"))

;;; Commentary:
;;
;; Provider-agnostic core for vegeta.  Defines the sidebar buffer, mode,
;; grouping engine, rendering, async parse loop, provider registry, and
;; cache.  Concrete providers (agent-shell, Claude CLI, ...) live in
;; sibling files and register themselves via `vegeta-register-provider'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'project)

(declare-function projectile-known-projects "projectile")
(defvar projectile-known-projects)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function fzfa-completing-read "fzfa" (&rest args))
(declare-function fzfa--read "fzfa" (sources &rest args))
(declare-function fzfa--ensure-setup "fzfa" ())
(declare-function fzfa--multi-allocate-narrow-keys "fzfa" (sources))

(declare-function evil-define-key* "evil-core")
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-goto-first-line "evil-commands")
(declare-function evil-goto-line "evil-commands")

(declare-function ghostel-exec "ghostel")
(declare-function vterm "vterm")
(defvar vterm-shell)

;;; Customization

(defgroup vegeta nil
  "Sidebar browser for AI agent chat transcripts across providers."
  :group 'tools)

(defcustom vegeta-name "*:Agent Chats:*"
  "Name of the sidebar buffer."
  :type 'string)

(defcustom vegeta-buffer-name "*vegeta*"
  "Name of the buffer used by `\\[vegeta]' (non-sidebar view).
Kept separate from `vegeta-name' so the sidebar and the full-frame
browser can coexist without stepping on each other's window
properties (dedicated flag, `window-size-fixed', etc.)."
  :type 'string)

(defcustom vegeta-width 55
  "Width of the sidebar window."
  :type 'integer)

(defcustom vegeta-display-alist '((side . left) (slot . 2))
  "Alist passed to `display-buffer-in-side-window'."
  :type 'alist)

(defcustom vegeta-pop-to-sidebar-on-toggle-open t
  "Whether to select the sidebar window after toggling it open."
  :type 'boolean)

(defcustom vegeta-no-delete-other-windows t
  "Whether the sidebar window survives `delete-other-windows'."
  :type 'boolean)

(defcustom vegeta-resize-on-open t
  "Whether to resize the sidebar window when opening it."
  :type 'boolean)

(defcustom vegeta-window-fixed 'width
  "Value for `window-size-fixed' in sidebar buffers."
  :type '(choice (const :tag "Not fixed" nil)
                 (const :tag "Fixed width" width)
                 (const :tag "Fixed height" height)))

(defcustom vegeta-parse-chunk-size 10
  "Number of entry headers to parse per idle tick.
Smaller values yield more often to user input; larger values finish
the background scan sooner at the cost of interactivity.
Only used by the fallback in-process parser; `vegeta-async-workers'
controls the batch size when async parsing is active."
  :type 'integer)

(defcustom vegeta-parse-idle-delay 0.3
  "Idle delay in seconds between parse chunks.
Only used by the fallback in-process parser."
  :type 'number)

(defcustom vegeta-async-workers 4
  "Number of parallel subprocess workers for metadata parsing.
When the `async' library is installed, `vegeta-refresh' splits the
parse queue into this many roughly-equal batches and dispatches each
to its own Emacs subprocess.  Each finished batch merges into the
in-memory cache and triggers a redraw; the main Emacs stays fully
responsive throughout.  Set to 0 or nil to force the fallback
in-process idle-timer parser."
  :type '(choice (const :tag "Disabled (in-process only)" nil)
                 integer))

(defcustom vegeta-open-file-in-most-recently-used-window t
  "Whether visited chats open in the MRU window."
  :type 'boolean)

(defcustom vegeta-refresh-timer 30
  "Auto-refresh the sidebar every N seconds when idle.  Nil disables."
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom vegeta-extra-project-roots nil
  "Additional project roots to scan.
Stray directories that the automatic project checks
\(`project-known-project-roots', Projectile, live agent-shell buffers)
do not catch — e.g. \"/bebe/script/af\".

On the local host these are merged with the detected project roots.  On
a remote `vegeta-hosts' machine they become the only roots scanned (the
project list of a remote machine is unknown), each resolved on that
host through Tramp.  So one entry both adds a local stray root and lets
a remote host be targeted directly:

  (setq vegeta-extra-project-roots (list \"/bebe/script/af\"))

Only project-root based providers consume these roots; today that is
`agent-shell' (which looks for `<root>/.agent-shell/transcripts/')."
  :type '(repeat directory))

(defcustom vegeta-collapse-empty-projects t
  "Hide projects with no entries from the sidebar."
  :type 'boolean)

(defcustom vegeta-grouping '(host package repo model date)
  "Grouping levels for the sidebar tree, outermost first.
Each element is one of the recognized levels:
  `host'    — group by the machine the chat was found on (see
              `vegeta-hosts'); skipped automatically when only
              `localhost' is configured
  `package' — group by provider (e.g. agent-shell, Claude CLI)
  `repo'    — group by project/repository root
  `model'   — group by agent/model name (Claude, Codex, ...)
  `date'    — group by date (from parsed timestamp or file mtime).  A
              relative, adaptive hierarchy: a year section appears only
              when entries span multiple years, a month section only for
              multiple months, and recent days show as Today / Yesterday
              / `N days ago'

An empty list produces a flat listing sorted by date."
  :type '(repeat (choice (const host)
                         (const package)
                         (const repo)
                         (const model)
                         (const date))))

(defcustom vegeta-date-granularity 'days
  "Leaf granularity for the `date' grouping level.
`days'  — one section per day; recent days are labelled Today,
          Yesterday, `N days ago'.
`weeks' — one section per week (This week / Last week / N weeks ago);
          each row then shows its own day.
The year and month sections above the leaf are added automatically only
when the entries actually span more than one of them."
  :type '(choice (const :tag "Days" days) (const :tag "Weeks" weeks))
  :group 'vegeta)

(defcustom vegeta-time-format "%-I:%M %p"
  "Time format used for entry rows and `(updated ...)' suffixes.
Default is a 12-hour clock with a space before AM/PM, e.g. \"10:59 PM\";
set to \"%H:%M\" for a 24-hour clock, or \"%l:%M %p\" for a
space-padded hour."
  :type 'string
  :group 'vegeta)

(defcustom vegeta-enabled-providers
  '(agent-shell claude-cli antigravity-cli antigravity eca)
  "Provider ids to include in the sidebar.
See `vegeta-providers' for available providers."
  :type '(repeat symbol))

(defcustom vegeta-hosts
  '((localhost . all))
  "Hosts to query for chats, and which providers to query on each.
An alist of (HOST . PROVIDERS):

  HOST      \"localhost\" for the local machine, or an SSH/Tramp
            target such as \"192.168.1.120\" or \"user@box.lan\".
  PROVIDERS a list of provider ids to query on that host, or the
            symbol `all' for every `vegeta-enabled-providers' entry.

For example:

  (setq vegeta-hosts
        `((localhost . all)
          (\"192.168.1.120\" . (claude-cli antigravity-cli))))

Remote hosts are reached through Tramp (`/ssh:HOST:'), so neither Emacs
nor vegeta needs to be installed on the host — only SSH access and the
provider's own data.  A provider that cannot operate remotely declares
`:local-only' and is skipped on remote hosts (e.g. one that shells out
to a local binary).  Leaving this at its default queries only the local
machine and changes nothing for existing users."
  :type '(alist :key-type (choice (string :tag "Host")
                                  (symbol :tag "Host name"))
                :value-type (choice (const all) (repeat symbol))))

(defcustom vegeta-terminal-function nil
  "Function to run PROGRAM with ARGS in a terminal at DIRECTORY.
Called as (NAME PROGRAM ARGS DIRECTORY).  When nil, `vegeta'
tries ghostel, then vterm, then `term'."
  :type '(choice (const :tag "Auto (ghostel > vterm > term)" nil)
                 function))

(defcustom vegeta-cache-file
  (expand-file-name ".vegeta-cache.eld" user-emacs-directory)
  "File where parsed metadata is persisted across sessions.
Loaded lazily on the first refresh; rewritten by an idle timer
whenever the in-memory cache is dirty and the parse queue is
drained."
  :type 'file)

(defcustom vegeta-cache-flush-idle-delay 2.0
  "Seconds of idle time before a dirty cache is flushed to disk."
  :type 'number)

(defcustom vegeta-updated-suffix-min-delta 300
  "Minimum seconds between start and last-update to show the update suffix.
When an entry's last-activity time is at least this many seconds after
its start time, the row shows a `(updated MM-DD HH:MM)' suffix so the
user can see the chat has continued past its opening prompt.  Set to
nil to always hide the suffix."
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom vegeta-delete-double-confirm-threshold 5
  "Ask a second confirmation when deleting more than this many chats.
Above the threshold, the first `yes-or-no-p' is followed by a second,
stronger one.  Set to nil to always confirm only once."
  :type '(choice (const :tag "Never double-confirm" nil) integer))

(defcustom vegeta-max-line-width 'window
  "Maximum characters per rendered chat row.
Entry titles are truncated with an ellipsis so the whole row (indent +
mark + date + optional update suffix + title) fits in this many
columns.

Values:
  `window'  — use the width of the window currently displaying the
              buffer.  Full-frame views get full width; a narrow
              sidebar clips titles to the sidebar's width.  Resizing
              the window triggers a redraw so titles re-fit.
  integer   — fixed width regardless of the window.
  nil       — no textual truncation; rely on `truncate-lines' for
              horizontal clipping."
  :type '(choice (const :tag "Window width (adaptive)" window)
                 (const :tag "Unlimited (rely on truncate-lines)" nil)
                 integer))

;;; Faces

(defface vegeta-package-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for provider/package header rows.")

(defface vegeta-project-face
  '((t :inherit dired-directory :weight bold))
  "Face for project header rows.")

(defface vegeta-agent-face
  '((t :inherit font-lock-keyword-face))
  "Face for agent/model header rows.")

(defface vegeta-date-face
  '((t :inherit font-lock-constant-face))
  "Face for entry date column.")

(defface vegeta-updated-face
  '((t :inherit shadow))
  "Face for the `(updated ...)' suffix on entry rows.")

(defface vegeta-mark-face
  '((t :inherit dired-mark))
  "Face for the generic mark (`*', dired's `m').")

(defface vegeta-flag-face
  '((t :inherit dired-flagged))
  "Face for the deletion flag (`D', dired's `d').")

(defface vegeta-placeholder-face
  '((t :inherit shadow))
  "Face for un-parsed placeholder metadata.")

(defface vegeta-host-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for host header rows (see `vegeta-hosts').")

;;; State

(defconst vegeta--cache-schema 6
  "Bump when the parsed metadata format changes to invalidate old entries.")

(defvar vegeta--parse-cache (make-hash-table :test 'equal)
  "Maps entry id -> (:schema INT :mtime FLOAT :meta PLIST).
The meta plist keys are:
  :agent         provider-supplied agent/model label string
  :model         provider-supplied model id string
  :started-at    conversation start timestamp string
  :updated-at    last-activity timestamp string, nil to fall back to mtime
  :cwd           working directory string
  :session-id    provider-resumable session id, or nil
  :first-prompt  user's first prompt (raw) or nil
  :ai-title      provider-generated summary title, or nil
  :renamed       user-supplied override title, or nil
The rendered row title is picked from (:renamed :ai-title :first-prompt)
in that order of priority.")

(defvar vegeta--parse-timer nil
  "Active idle timer for chunked metadata parsing.")

(defvar vegeta--parse-queue nil
  "List of entries awaiting metadata parse.")

(defvar vegeta--parse-queued-ids (make-hash-table :test 'equal)
  "Hash set of entry ids currently on `vegeta--parse-queue'.
Used for O(1) duplicate suppression when queuing.")

(defvar vegeta--entries-cache nil
  "Discovery result cached across `vegeta--redraw' calls.
Cleared by `vegeta-refresh' so background parse ticks don't
re-scan the filesystem on every chunk.")

(defvar vegeta--current-host "localhost"
  "Host currently being queried.
Bound to the target host while a provider's hooks run so file paths
resolve locally or over Tramp, and read by `vegeta--launch-terminal'
to decide between a local and an ssh session.")

(defvar vegeta--date-levels nil
  "Adaptive `date' sublevels in effect for the current redraw.
An ordered subset of `(date-year date-month date-day date-week)',
computed from the entries' spread; consulted when labelling date
sections and entry rows.")

(defvar vegeta--async-pending 0
  "Number of async worker batches still in flight.
When it hits zero after a refresh, the persisted cache flushes to disk.")

(defvar vegeta--host-jobs 0
  "Number of background remote-host listing jobs still in flight.")

(defvar vegeta--pending-hosts nil
  "Hosts with a background listing job currently in flight.")

(declare-function async-start "async")

(defvar-local vegeta--marks nil
  "Hash table mapping entry id -> mark symbol (e.g. `delete').")

(defvar-local vegeta--collapsed nil
  "Hash table of collapsed group breadcrumbs (list of level keys).")

(defvar-local vegeta--rendered-width nil
  "Effective row width used by the last redraw, or nil when untruncated.
Lets `vegeta--window-size-changed' skip a redraw when a size-change
event finds the displaying window at the width already rendered.")

(defvar-local vegeta--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

(defvar vegeta--cache-loaded nil
  "Non-nil once the on-disk cache has been loaded (or attempted).")

(defvar vegeta--cache-dirty nil
  "Non-nil when in-memory cache has changes not yet flushed to disk.")

(defvar vegeta--cache-flush-timer nil
  "Active idle timer for flushing the cache to disk.")

;;; Project roots

(defun vegeta--live-agent-shell-roots ()
  "Return project roots used by live agent-shell buffers."
  (when (fboundp 'agent-shell-buffers)
    (delq nil
          (mapcar
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (ignore-errors
                   (file-name-as-directory
                    (expand-file-name (agent-shell-cwd)))))))
           (agent-shell-buffers)))))

(defun vegeta--project-roots ()
  "Return the project roots to scan for `vegeta--current-host'.
On the local host, the union of the detected project roots
\(`project-known-project-roots', Projectile, live agent-shell buffers)
plus `vegeta-extra-project-roots'.  On a remote host, whose own project
list is unknown, only `vegeta-extra-project-roots' — each resolved on
that host so the scan runs over Tramp."
  (if (vegeta--local-host-p vegeta--current-host)
      (let ((roots (append
                    (when (fboundp 'project-known-project-roots)
                      (project-known-project-roots))
                    (when (bound-and-true-p projectile-known-projects)
                      projectile-known-projects)
                    (vegeta--live-agent-shell-roots)
                    vegeta-extra-project-roots)))
        (thread-last roots
                     (mapcar (lambda (r)
                               (when r
                                 (expand-file-name (file-name-as-directory r)))))
                     (delq nil)
                     (seq-uniq)))
    (thread-last vegeta-extra-project-roots
                 (mapcar (lambda (r)
                           (when r
                             (file-name-as-directory
                              (vegeta--host-join vegeta--current-host r)))))
                 (delq nil)
                 (seq-uniq))))

(defun vegeta--disambiguated-names (roots)
  "Return a hash table mapping each ROOT to a unique display name.
Uses the shortest trailing path suffix that is unique across ROOTS."
  (let* ((splits (mapcar (lambda (r)
                           (cons r
                                 (nreverse
                                  (split-string
                                   (directory-file-name r) "/" t))))
                         roots))
         (depths (make-hash-table :test 'equal))
         (result (make-hash-table :test 'equal)))
    (dolist (pair splits)
      (puthash (car pair) 1 depths))
    (let ((changed t))
      (while changed
        (setq changed nil)
        (let ((by-name (make-hash-table :test 'equal)))
          (dolist (pair splits)
            (let* ((r (car pair))
                   (parts (cdr pair))
                   (d (min (gethash r depths) (length parts)))
                   (name (string-join
                          (nreverse (seq-take parts d)) "/")))
              (push r (gethash name by-name))))
          (maphash
           (lambda (_name rs)
             (when (> (length rs) 1)
               (dolist (r rs)
                 (let* ((pair (assoc r splits))
                        (parts (cdr pair))
                        (d (gethash r depths)))
                   (when (< d (length parts))
                     (puthash r (1+ d) depths)
                     (setq changed t))))))
           by-name))))
    (dolist (pair splits)
      (let* ((r (car pair))
             (parts (cdr pair))
             (d (min (gethash r depths) (length parts))))
        (puthash r
                 (string-join (nreverse (seq-take parts d)) "/")
                 result)))
    result))

;;; Hosts
;;
;; A "host" is where a provider's data lives: "localhost" (the local
;; filesystem) or an SSH target.  Remote hosts are read through Tramp,
;; so a provider only needs to expose its data roots via `:host-dirs'
;; (an alist of (VARIABLE . "~/relative/path")); vegeta rebinds those
;; variables to `/ssh:HOST:~/...' roots while the provider runs, and the
;; provider's existing file operations then transparently go over SSH.

(defvar vegeta-providers)
(defconst vegeta--local-host "localhost"
  "Name used for the local machine in `vegeta-hosts'.")

(defun vegeta--host-name (host)
  "Return HOST as a normalized string."
  (cond ((symbolp host) (symbol-name host))
        ((stringp host) host)
        (t (format "%s" host))))

(defun vegeta--local-host-p (host)
  "Return non-nil when HOST names the local machine."
  (member (vegeta--host-name host) '("localhost" "127.0.0.1" "::1")))

(defun vegeta--hosts ()
  "Return the configured hosts as a list of (HOST-NAME . PROVIDERS).
Falls back to localhost/all when `vegeta-hosts' is nil or empty."
  (let ((hosts (if vegeta-hosts vegeta-hosts '((localhost . all)))))
    (mapcar (lambda (pair)
              (cons (vegeta--host-name (car pair)) (cdr pair)))
            hosts)))

(defun vegeta--remote-hosts ()
  "Return the configured non-local host names."
  (seq-remove #'vegeta--local-host-p (mapcar #'car (vegeta--hosts))))

(defun vegeta--providers-for-host (host-spec)
  "Return the enabled provider plists to query for HOST-SPEC.
HOST-SPEC is a list of provider ids or the symbol `all'."
  (let ((ids (if (eq host-spec 'all)
                 vegeta-enabled-providers
               host-spec)))
    (delq nil
          (mapcar (lambda (id) (alist-get id vegeta-providers))
                  (seq-filter (lambda (id) (memq id vegeta-enabled-providers))
                              ids)))))

(defun vegeta--entry-host (entry)
  "Return ENTRY's host as a string, defaulting to localhost."
  (vegeta--host-name (or (plist-get entry :host) vegeta--local-host)))

(defun vegeta--host-tramp-prefix (host)
  "Return the Tramp prefix for remote HOST, e.g. \"/ssh:box:\"."
  (format "/ssh:%s:" (vegeta--host-name host)))

(defun vegeta--host-join (host path)
  "Resolve PATH for HOST.
For localhost PATH is expanded against the local home; for a remote
host it becomes a Tramp `/ssh:HOST:PATH' name.  PATH is expected to be
`~'-relative (so it resolves to the remote home), though an absolute
remote path works too."
  (if (vegeta--local-host-p host)
      (expand-file-name path)
    (let ((prefix (vegeta--host-tramp-prefix host)))
      (if (string-prefix-p prefix path) path (concat prefix path)))))

(defun vegeta--hostify (host path)
  "Return PATH tramp-qualified for a remote HOST, or unchanged locally.
Already-qualified paths are returned as-is; non-string values (nil,
numbers) pass through."
  (cond
   ((null path) nil)
   ((not (stringp path)) path)
   ((vegeta--local-host-p host) path)
   ((string-prefix-p "/ssh:" path) path)
   (t (vegeta--host-join host path))))

(defun vegeta--unhostify (host path)
  "Strip HOST's Tramp prefix from PATH, yielding the host-native path."
  (if (and (stringp path)
           (string-prefix-p (vegeta--host-tramp-prefix host) path))
      (substring path (length (vegeta--host-tramp-prefix host)))
    path))

(defun vegeta--hostify-meta (meta host)
  "Tramp-qualify path-valued fields of META for a remote HOST.
Providers parse native paths out of a remote file (e.g. a transcript's
recorded cwd), so hostify them here to keep downstream file operations
correct.  Returns META."
  (when (and meta (not (vegeta--local-host-p host)))
    (when-let* ((cwd (plist-get meta :cwd)))
      (plist-put meta :cwd (vegeta--hostify host cwd))))
  meta)

(defun vegeta--call-with-host (provider host fn &rest args)
  "Call FN (a provider hook) with ARGS, resolving paths for HOST.
For a remote HOST the provider's `:host-dirs' variables are bound to
host-qualified roots so its file operations run over Tramp; for
localhost the user's configured values are used untouched.  Errors from
an unreachable remote host are reported once and yield nil."
  (let ((vegeta--current-host host))
    (if (vegeta--local-host-p host)
        (apply fn args)
      (condition-case err
          (let ((dirs (delq nil (plist-get provider :host-dirs))))
            (if dirs
                (cl-progv (mapcar #'car dirs)
                          (mapcar (lambda (p) (vegeta--host-join host (cdr p)))
                                  dirs)
                  (apply fn args))
              (apply fn args)))
        (error
         (message "vegeta: host %s: %s" host (error-message-string err))
         nil)))))

;;; Time utilities

(defun vegeta--iso-to-seconds (str)
  "Parse timestamp STR to seconds-since-epoch, or nil.
Accepts both agent-shell's local-time format and Claude's ISO UTC form."
  (condition-case _
      (float-time (date-to-time str))
    (error nil)))

;;; Terminal launcher

(defcustom vegeta-remote-ssh-args '()
  "Extra arguments passed to `ssh' when launching a remote host's chat.
The host and the remote command are appended after these."
  :type '(repeat string)
  :group 'vegeta)

(defun vegeta--pop-terminal (name program args directory)
  "Show PROGRAM ARGS in a terminal buffer named NAME at DIRECTORY."
  (if vegeta-terminal-function
      (funcall vegeta-terminal-function name program args directory)
    (cond
     ((fboundp 'ghostel-exec)
      (let* ((default-directory (file-name-as-directory directory))
             (buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (ghostel-exec buffer program args))
        (vegeta--pop-to buffer)
        buffer))
     ((fboundp 'vterm)
      (let* ((default-directory (file-name-as-directory directory))
             (vterm-shell (mapconcat #'shell-quote-argument
                                     (cons program args) " ")))
        (funcall (symbol-function 'vterm) name)))
     (t
      (let ((default-directory (file-name-as-directory directory)))
        (term (mapconcat #'shell-quote-argument
                         (cons program args) " ")))))))

(defun vegeta--launch-terminal (name program args directory)
  "Launch PROGRAM ARGS in a terminal buffer named NAME at DIRECTORY.
DIRECTORY is a path on `vegeta--current-host'; when that host is
remote, PROGRAM is run there through `ssh -t' (a login shell so
interactive CLIs work), starting in DIRECTORY."
  (let ((host vegeta--current-host))
    (if (vegeta--local-host-p host)
        (vegeta--pop-terminal name program args
                              (file-name-as-directory directory))
      (let* ((native-dir (vegeta--unhostify host directory))
             (remote-command
              (format "cd %s && exec %s"
                      (shell-quote-argument native-dir)
                      (mapconcat #'shell-quote-argument
                                 (cons program args) " "))))
        (vegeta--pop-terminal
         name "ssh"
         (append vegeta-remote-ssh-args
                 (list "-t" host remote-command))
         default-directory)))))

;;; Provider protocol
;;
;; A provider is a plist with the keys:
;;   :id           symbol, e.g. `agent-shell'                          (required)
;;   :name         display string, e.g. "Agent Shell"                  (required)
;;   :list         () -> list of ENTRY plists                          (required)
;;   :parse        (ENTRY) -> META plist                               (required)
;;   :visit        (ENTRY) -> side-effect (opens/resumes)              (required)
;;   :date-key     (ENTRY) -> "YYYY-MM-DD" for date grouping           (optional)
;;   :delete       (ENTRY) -> removes the entry's underlying storage   (optional)
;;   :open-transcript (ENTRY) -> buffer of a rendered transcript       (optional)
;;                 used by `vegeta-open-transcript' before falling
;;                 back to opening the entry's `:id' file
;;   :open-search-result (ENTRY) -> buffer for a full-text search hit  (optional)
;;                 defaults to `:open-transcript'; lets a provider whose
;;                 storage isn't line-addressable (e.g. ECA's Transit
;;                 files) open a search match as the readable chat
;;   :search-command (HOST SCOPE TARGETS) -> shell command string      (optional)
;;                 full-text search; the string is streamed by `fzfa'
;;                 (spawned locally for localhost, ssh-wrapped for a
;;                 remote host).  SCOPE is one of `vegeta-search-scope';
;;                 TARGETS is nil (search the provider's default roots)
;;                 or a list of native paths to restrict to (e.g. the
;;                 marked chats)
;;   :host-dirs    alist of (VARIABLE . "~/relative/path")            (optional)
;;                 data roots; rebound to `/ssh:HOST:...' so the
;;                 provider can run against a remote `vegeta-hosts'
;;                 entry over Tramp
;;   :local-only   t when the provider cannot run on a remote host    (optional)
;;                 (e.g. it shells out to a local binary); such a
;;                 provider is skipped for remote hosts
;;   :skip-levels  list of level symbols to omit from grouping for
;;                 this provider's entries (e.g. `(model)' for a
;;                 single-agent provider like `claude-cli')            (optional)
;;
;; An ENTRY is a plist with:
;;   :provider SYMBOL             (provider id)
;;   :id       STRING             (globally unique; usually the source file path)
;;   :host     STRING             (host it was found on; default "localhost")
;;   :repo     STRING or nil      (project root path)
;;   :agent    STRING or nil      (fast-known agent/model label; else parsed)
;;   :mtime    FLOAT              (for sorting and cache validation)
;;   :extras   PLIST              (provider-specific data, e.g. session-id)
;;
;; A META plist returned by :parse has keys:
;;   :agent :model :started-at :updated-at :cwd :session-id
;;   :first-prompt :ai-title :renamed
;; See the docstring of `vegeta--parse-cache' for the semantics of each.

(defvar vegeta-providers nil
  "Alist of (ID . PROVIDER-PLIST).")

(defun vegeta-register-provider (provider)
  "Register or replace PROVIDER (a plist).
See the commentary in `vegeta-core.el' for the required keys."
  (let ((id (plist-get provider :id)))
    (unless (symbolp id)
      (error "Provider must have a symbol :id, got %S" id))
    (dolist (k '(:name :list :parse :visit))
      (unless (plist-get provider k)
        (error "Provider %s is missing required key %s" id k)))
    (setq vegeta-providers
          (cons (cons id provider)
                (assq-delete-all id vegeta-providers)))))

(defun vegeta--enabled-providers ()
  "Return the enabled providers as plists in configured order."
  (delq nil
        (mapcar (lambda (id) (alist-get id vegeta-providers))
                vegeta-enabled-providers)))

(defun vegeta--entry-provider (entry)
  "Return the provider plist for ENTRY."
  (alist-get (plist-get entry :provider) vegeta-providers))

(defun vegeta--default-delete (entry)
  "Default `:delete' fallback: remove the file at ENTRY's `:id'.
Providers whose storage lives in a single file don't need to override
this — it's the right thing.  Providers that keep sidecar files (a
per-session env dir, an index entry, etc.) should implement `:delete'."
  (let ((id (plist-get entry :id)))
    (when (and id (file-exists-p id))
      (delete-file id))))

;;; Cache

(defun vegeta--cached-meta (entry)
  "Return cached metadata plist for ENTRY if fresh, else nil."
  (when-let* ((id (plist-get entry :id))
              (mtime (plist-get entry :mtime))
              (cell (gethash id vegeta--parse-cache))
              (schema (plist-get cell :schema))
              ((equal schema vegeta--cache-schema))
              (cached-mtime (plist-get cell :mtime))
              ((equal cached-mtime mtime)))
    (plist-get cell :meta)))

(defun vegeta--put-cache (entry meta)
  "Store META for ENTRY in the parse cache."
  (puthash (plist-get entry :id)
           (list :schema vegeta--cache-schema
                 :mtime (plist-get entry :mtime)
                 :meta meta)
           vegeta--parse-cache)
  (vegeta--cache-mark-dirty))

(defun vegeta--ensure-parsed (entry)
  "Return metadata for ENTRY, parsing synchronously on cache miss."
  (or (vegeta--cached-meta entry)
      (let* ((provider (vegeta--entry-provider entry))
             (host (vegeta--entry-host entry))
             (parser (plist-get provider :parse))
             (meta (and parser
                        (vegeta--hostify-meta
                         (vegeta--call-with-host provider host parser entry)
                         host))))
        (vegeta--put-cache entry meta)
        meta)))

;;; Cache persistence

(defun vegeta--cache-load ()
  "Load persisted metadata cache into `vegeta--parse-cache'.
Silent no-op when the cache file doesn't exist.  Cache entries whose
schema version differs from the current `vegeta--cache-schema' are
skipped so parser changes invalidate stale data automatically."
  (setq vegeta--cache-loaded t)
  (when (file-readable-p vegeta-cache-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents vegeta-cache-file)
          (goto-char (point-min))
          (let* ((data (read (current-buffer)))
                 (entries (plist-get data :entries))
                 (loaded 0))
            (dolist (row entries)
              (let* ((id (car row))
                     (cell (cdr row))
                     (schema (plist-get cell :schema)))
                (when (equal schema vegeta--cache-schema)
                  (puthash id cell vegeta--parse-cache)
                  (cl-incf loaded))))
            (message "vegeta: loaded %d cached entries from %s"
                     loaded vegeta-cache-file)))
      (error
       (message "vegeta: failed to load cache file %s: %S"
                vegeta-cache-file err)))))

(defun vegeta--cache-save ()
  "Write `vegeta--parse-cache' to `vegeta-cache-file' atomically."
  (condition-case err
      (let (entries)
        (maphash (lambda (id cell) (push (cons id cell) entries))
                 vegeta--parse-cache)
        (make-directory (file-name-directory vegeta-cache-file) t)
        (with-temp-file vegeta-cache-file
          (let ((print-length nil)
                (print-level nil)
                (print-circle nil))
            (insert ";; vegeta metadata cache -- auto-generated, do not edit by hand\n")
            (prin1 (list :vegeta-schema vegeta--cache-schema
                         :written-at (float-time)
                         :count (length entries)
                         :entries entries)
                   (current-buffer))
            (insert "\n")))
        (setq vegeta--cache-dirty nil))
    (error
     (message "vegeta: failed to save cache file %s: %S"
              vegeta-cache-file err))))

(defun vegeta--cache-mark-dirty ()
  "Mark the in-memory cache as dirty and schedule a background flush."
  (setq vegeta--cache-dirty t)
  (unless (timerp vegeta--cache-flush-timer)
    (setq vegeta--cache-flush-timer
          (run-with-idle-timer
           vegeta-cache-flush-idle-delay t
           #'vegeta--cache-flush-tick))))

(defun vegeta--cache-flush-tick ()
  "Idle-timer tick: flush cache to disk when dirty and work has drained."
  (when (and vegeta--cache-dirty
             (null vegeta--parse-queue)
             (zerop vegeta--host-jobs))
    (vegeta--cache-save))
  ;; Stop rescheduling once nothing is dirty and no async work remains.
  (when (and (not vegeta--cache-dirty)
             (null vegeta--parse-queue)
             (zerop vegeta--host-jobs))
    (when (timerp vegeta--cache-flush-timer)
      (cancel-timer vegeta--cache-flush-timer))
    (setq vegeta--cache-flush-timer nil)))

;;; Async parse loop

(defun vegeta--queue-uncached (entries)
  "Push ENTRIES lacking a fresh cache entry onto the parse queue.
O(n) via a hash-set of queued ids — the previous implementation used
`member' + `nconc' which was O(n^2) in queue length and stalled Emacs
for seconds on a several-hundred-entry initial scan."
  (dolist (e entries)
    (let ((id (plist-get e :id)))
      (unless (or (vegeta--cached-meta e)
                  (gethash id vegeta--parse-queued-ids))
        (puthash id t vegeta--parse-queued-ids)
        (push e vegeta--parse-queue)))))

(defun vegeta--start-parse-timer ()
  "Kick off the idle timer if there is work to do and it's not running."
  (when (and vegeta--parse-queue
             (not (timerp vegeta--parse-timer)))
    (setq vegeta--parse-timer
          (run-with-idle-timer
           vegeta-parse-idle-delay t
           #'vegeta--parse-tick))))

;;; Async parsing (subprocess workers)

(defun vegeta--async-available-p ()
  "Return non-nil when async parsing is both configured and installed."
  (and vegeta-async-workers
       (> vegeta-async-workers 0)
       (require 'async nil t)))

(defun vegeta--split-list (lst n)
  "Split LST into at most N roughly equal chunks.
Returns fewer chunks when LST has fewer elements than N."
  (let* ((len (length lst))
         (chunk (max 1 (ceiling (/ (float len) (max 1 n)))))
         result)
    (while lst
      (push (seq-take lst chunk) result)
      (setq lst (nthcdr chunk lst)))
    (nreverse result)))

(defun vegeta--redraw-all-sidebars ()
  "Redraw every live vegeta sidebar buffer."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'vegeta-mode)
          (vegeta--redraw))))))

(defcustom vegeta-async-provider-modules
  '(vegeta-agent-shell
    vegeta-claude-cli
    vegeta-antigravity-cli
    vegeta-antigravity
    vegeta-eca)
  "Provider feature symbols each async worker `require's before parsing.
Every built-in provider file must be listed here so its `:parse'
implementation is registered in the worker; if it isn't, entries from
that provider skip parsing silently and render forever as placeholders.
Add third-party provider modules here to get the same async treatment."
  :type '(repeat symbol)
  :group 'vegeta)

(defun vegeta--async-worker-form (batch lib-dir)
  "Return the lambda form the async worker should evaluate.
BATCH is the entry list to parse; LIB-DIR is the directory containing
`vegeta-core.el' so the worker can add it to its load-path."
  (let ((modules vegeta-async-provider-modules))
    `(lambda ()
       ;; The worker inherits none of the parent's state.  Load just
       ;; enough of vegeta to register providers and reach the parsers;
       ;; we deliberately avoid `package-initialize' since parsing needs
       ;; no ELPA packages (agent-shell is lazy-required by :visit).
       (setq load-path (cons ,lib-dir load-path))
       (require 'vegeta-core)
       (dolist (m ',modules)
         (require m))
       (mapcar
        (lambda (entry)
          (let* ((prov (alist-get (plist-get entry :provider)
                                  vegeta-providers))
                 (parser (plist-get prov :parse))
                 (host (or (plist-get entry :host) "localhost")))
            ;; Return a compact tuple; the caller writes it into
            ;; `vegeta--parse-cache' with the current schema version.
            (list (plist-get entry :id)
                  (plist-get entry :mtime)
                  (and parser
                       (vegeta--hostify-meta
                        (vegeta--call-with-host prov host parser entry)
                        host)))))
        ',batch))))

(defun vegeta--async-callback (results)
  "Merge RESULTS from an async worker into the cache, then redraw."
  (dolist (row results)
    (let ((id (nth 0 row))
          (mtime (nth 1 row))
          (meta (nth 2 row)))
      (puthash id (list :schema vegeta--cache-schema
                        :mtime mtime
                        :meta meta)
               vegeta--parse-cache)))
  (setq vegeta--cache-dirty t)
  (setq vegeta--async-pending (max 0 (1- vegeta--async-pending)))
  (vegeta--redraw-all-sidebars)
  (when (zerop vegeta--async-pending)
    ;; Arm the flush timer so the just-parsed batch persists.
    (unless (timerp vegeta--cache-flush-timer)
      (setq vegeta--cache-flush-timer
            (run-with-idle-timer
             vegeta-cache-flush-idle-delay t
             #'vegeta--cache-flush-tick)))))

(defun vegeta--dispatch-async-batch (batch lib-dir)
  "Fire a single async worker for BATCH; results merge back on completion."
  (cl-incf vegeta--async-pending)
  (async-start (vegeta--async-worker-form batch lib-dir)
               #'vegeta--async-callback))

(defun vegeta--lib-dir ()
  "Return the directory holding `vegeta-core', for async workers."
  (file-name-directory
   (or (locate-library "vegeta-core")
       (error "vegeta-core.el not on load-path"))))

(defun vegeta--start-async-parse ()
  "Dispatch the current parse queue across `vegeta-async-workers'."
  (let* ((queue vegeta--parse-queue)
         (lib-dir (vegeta--lib-dir)))
    (setq vegeta--parse-queue nil)
    (clrhash vegeta--parse-queued-ids)
    (dolist (batch (vegeta--split-list queue vegeta-async-workers))
      (when batch
        (vegeta--dispatch-async-batch batch lib-dir)))))

(defun vegeta--maybe-start-parse ()
  "Start parsing the queue, in background workers when available."
  (cond
   ((null vegeta--parse-queue) nil)
   ((vegeta--async-available-p) (vegeta--start-async-parse))
   (t (vegeta--start-parse-timer))))

;;; Async host listing
;;
;; Local hosts are listed synchronously (fast).  Remote hosts are listed
;; in `async' subprocesses so a slow or unreachable machine cannot block
;; the refresh; results merge into `vegeta--entries-cache' and redraw the
;; sidebar as they arrive.

(defun vegeta--async-host-form (host provider-ids lib-dir extra-roots)
  "Return the worker lambda that lists HOST's PROVIDER-IDS.
LIB-DIR is where `vegeta-core.el' lives; EXTRA-ROOTS is
`vegeta-extra-project-roots' so project-root providers can resolve
their roots on HOST over Tramp."
  (let ((modules vegeta-async-provider-modules))
    `(lambda ()
       (setq load-path (cons ,lib-dir load-path))
       (require 'vegeta-core)
       (dolist (m ',modules)
         (require m))
       (condition-case err
           (let ((vegeta--current-host ,host)
                 (vegeta-extra-project-roots ',extra-roots))
             (let (all)
               (dolist (id ',provider-ids)
                 (let ((prov (alist-get id vegeta-providers)))
                   (when (and prov (plist-get prov :list))
                     (setq all (append all
                                       (vegeta--call-with-host
                                        prov ,host (plist-get prov :list)))))))
               (mapcar (lambda (entry)
                         (vegeta--tag-host-entry entry ,host))
                       all)))
         (error
          (message "vegeta: host job %s failed: %S" ,host err)
          nil)))))

(defun vegeta--dispatch-host-job (host providers)
  "Start a background listing job for HOST's PROVIDERS."
  (let ((ids (delq nil (mapcar (lambda (p) (plist-get p :id)) providers))))
    (cl-incf vegeta--host-jobs)
    (push host vegeta--pending-hosts)
    (async-start
     (vegeta--async-host-form host ids (vegeta--lib-dir)
                              vegeta-extra-project-roots)
     (lambda (results) (vegeta--host-callback host results)))))

(defun vegeta--host-callback (host results)
  "Merge RESULTS from HOST's listing job, then redraw."
  (setq vegeta--pending-hosts (delete host vegeta--pending-hosts))
  (setq vegeta--host-jobs (max 0 (1- vegeta--host-jobs)))
  (when (listp results)
    (setq vegeta--entries-cache (append vegeta--entries-cache results))
    (vegeta--queue-uncached results)
    (vegeta--redraw-all-sidebars))
  (when (zerop vegeta--host-jobs)
    (vegeta--maybe-start-parse)
    (unless (timerp vegeta--cache-flush-timer)
      (setq vegeta--cache-flush-timer
            (run-with-idle-timer vegeta-cache-flush-idle-delay t
                                 #'vegeta--cache-flush-tick)))))

(defun vegeta--remote-providers (host-spec)
  "Return the non-`:local-only' enabled providers for HOST-SPEC."
  (seq-remove (lambda (p) (plist-get p :local-only))
              (vegeta--providers-for-host host-spec)))

(defun vegeta--start-host-jobs ()
  "Dispatch a background listing job for each remote host."
  (dolist (pair (vegeta--hosts))
    (let ((host (car pair)))
      (unless (or (vegeta--local-host-p host)
                  (member host vegeta--pending-hosts))
        (let ((providers (vegeta--remote-providers (cdr pair))))
          (when providers
            (vegeta--dispatch-host-job host providers)))))))

(defun vegeta--start-host-jobs-sync ()
  "List each remote host's providers synchronously (no `async')."
  (dolist (pair (vegeta--hosts))
    (let ((host (car pair)))
      (unless (vegeta--local-host-p host)
        (dolist (provider (vegeta--remote-providers (cdr pair)))
          (setq vegeta--entries-cache
                (append vegeta--entries-cache
                        (vegeta--list-provider provider host))))))))

(defun vegeta--parse-tick ()
  "Parse the next chunk of entries, then redraw affected sidebars.
Wrapped in `while-no-input' so user input aborts the tick cleanly —
whatever entries were parsed before the abort stay cached, and the
next idle tick continues where we left off.  Any entry popped from
the queue but not parsed due to an abort gets requeued."
  (let ((aborted
         (while-no-input
           (let ((n 0) (parsed nil))
             (while (and vegeta--parse-queue
                         (< n vegeta-parse-chunk-size)
                         (not (input-pending-p)))
               (let* ((entry (pop vegeta--parse-queue))
                      (id (plist-get entry :id)))
                 (remhash id vegeta--parse-queued-ids)
                 (vegeta--ensure-parsed entry)
                 (push entry parsed))
               (cl-incf n))
             (when parsed
               (dolist (buf (buffer-list))
                 (when (and (buffer-live-p buf)
                            (eq (buffer-local-value 'major-mode buf)
                                'vegeta-mode))
                   (with-current-buffer buf
                     (vegeta--redraw)))))
             nil))))
    ;; `while-no-input' returns `t' on abort — nothing to do beyond
    ;; noting that some entries may have been popped but not parsed;
    ;; they'll be re-queued by the next `vegeta-refresh'.
    (ignore aborted)
    (unless vegeta--parse-queue
      (when (timerp vegeta--parse-timer)
        (cancel-timer vegeta--parse-timer))
      (setq vegeta--parse-timer nil))))

;;; Discovery aggregation

(defun vegeta--local-host-entries ()
  "Return entries for the local machine from its configured providers.
Iterates the `localhost' entries of `vegeta-hosts' (which may restrict
providers), running each provider in-process."
  (let (all)
    (dolist (pair (vegeta--hosts))
      (when (vegeta--local-host-p (car pair))
        (dolist (provider (vegeta--providers-for-host (cdr pair)))
          (setq all (append all
                            (vegeta--list-provider provider (car pair)))))))
    all))

(defun vegeta--all-entries ()
  "Return the current discovery result.
Populated by `vegeta-refresh' — local entries synchronously, remote
hosts merged in as their background jobs finish — and cached in
`vegeta--entries-cache' until the next refresh.  When unset, computes
only the local entries so a redraw before the first refresh still
works."
  (or vegeta--entries-cache
      (setq vegeta--entries-cache (vegeta--local-host-entries))))

(defun vegeta--list-provider (provider host)
  "Return PROVIDER's entries for HOST, tagged and host-qualified.
A provider flagged `:local-only' is skipped on remote hosts."
  (let ((lister (plist-get provider :list)))
    (cond
     ((null lister) nil)
     ((and (not (vegeta--local-host-p host))
           (plist-get provider :local-only))
      (message "vegeta: skipping local-only provider %s on host %s"
               (plist-get provider :id) host)
      nil)
     (t
      (mapcar (lambda (entry) (vegeta--tag-host-entry entry host))
              (vegeta--call-with-host provider host lister))))))

(defun vegeta--tag-host-entry (entry host)
  "Attach HOST to ENTRY and tramp-qualify its paths for a remote host."
  (plist-put entry :host host)
  (when (not (vegeta--local-host-p host))
    (plist-put entry :id (vegeta--hostify host (plist-get entry :id)))
    (when-let* ((repo (plist-get entry :repo)))
      (plist-put entry :repo (vegeta--hostify host repo))))
  entry)

;;; Entry accessors (consult cache when available)

(defun vegeta--entry-field (entry key)
  "Return KEY from ENTRY, preferring parsed metadata when cached."
  (let ((meta (vegeta--cached-meta entry)))
    (or (and meta (plist-get meta key))
        (plist-get entry key))))

(defun vegeta--entry-agent (entry)
  "Return the agent/model label for ENTRY (parsed or fallback)."
  (or (vegeta--entry-field entry :agent)
      (plist-get entry :agent)))

(defun vegeta--entry-preview (entry)
  "Return the parsed first-prompt preview for ENTRY, or nil if not yet parsed."
  (let ((meta (vegeta--cached-meta entry)))
    (and meta (plist-get meta :first-prompt))))

(defun vegeta--entry-title (entry)
  "Return the display title for ENTRY, or nil if not yet parsed.
Priority order: `:renamed' (user override), `:ai-title' (provider
summary), `:first-prompt' (raw user prompt)."
  (let ((meta (vegeta--cached-meta entry)))
    (and meta
         (or (plist-get meta :renamed)
             (plist-get meta :ai-title)
             (plist-get meta :first-prompt)))))

(defun vegeta--entry-started-at-seconds (entry)
  "Return ENTRY's `:started-at' as float-time, or nil."
  (let* ((meta (vegeta--cached-meta entry))
         (raw (and meta (plist-get meta :started-at))))
    (and raw (vegeta--iso-to-seconds raw))))

(defun vegeta--entry-updated-at-seconds (entry)
  "Return ENTRY's effective last-activity time as float-time, or nil.
Prefers meta `:updated-at' when a provider supplied it; otherwise falls
back to the filesystem mtime already carried on the entry."
  (let* ((meta (vegeta--cached-meta entry))
         (raw (and meta (plist-get meta :updated-at))))
    (or (and raw (vegeta--iso-to-seconds raw))
        (plist-get entry :mtime))))

(defun vegeta--entry-timestamp (entry)
  "Return the display timestamp for ENTRY.
When `date' groups the tree, the section headers carry the date, so a
row shows just its time — except under `weeks' granularity, where each
row also shows its day (the week section does not)."
  (let* ((meta (vegeta--cached-meta entry))
         (ts (and meta (plist-get meta :started-at)))
         (secs (or (and ts (vegeta--iso-to-seconds ts))
                   (plist-get entry :mtime)))
         (date-grouped (memq 'date vegeta-grouping)))
    (cond
     ((null secs)
      (if date-grouped "??:??" "??-?? ??:??"))
     ((null date-grouped)
      (format-time-string (concat "%m-%d " vegeta-time-format)
                          (seconds-to-time secs)))
     (t
      (format-time-string
       (if (vegeta--date-sectioned-p 'date-day)
           vegeta-time-format
         (concat "%d " vegeta-time-format))
       (seconds-to-time secs))))))

;;; Grouping engine

(defun vegeta--default-date-key (entry)
  "Default `:date-key' implementation for providers that don't supply one.
Uses parsed `:started-at' when available, else file mtime, else `unknown'."
  (let* ((meta (vegeta--cached-meta entry))
         (ts (and meta (plist-get meta :started-at))))
    (cond
     ((and ts (string-match
               "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
               ts))
      (match-string 1 ts))
     (t
      (let ((mtime (plist-get entry :mtime)))
        (if mtime
            (format-time-string "%Y-%m-%d" (seconds-to-time mtime))
          "unknown"))))))

(defun vegeta--entry-date-key (entry)
  "Return a YYYY-MM-DD string for grouping ENTRY by date.
Dispatches to the entry's provider `:date-key' function when defined.
Providers should return a deterministic value that does not depend on
whether the entry has been parsed yet — otherwise entries drift between
groups as background parsing completes."
  (let* ((provider (vegeta--entry-provider entry))
         (fn (and provider (plist-get provider :date-key))))
    (vegeta--call-with-host provider
                            (vegeta--entry-host entry)
                            (or fn #'vegeta--default-date-key)
                            entry)))

(defun vegeta--date-key-date-p (key)
  "Return non-nil when KEY is a full YYYY-MM-DD date string."
  (and (stringp key)
       (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" key)))

(defun vegeta--days-ago (key)
  "Return whole days from date KEY (YYYY-MM-DD) to today, or nil."
  (when (vegeta--date-key-date-p key)
    (condition-case nil
        (- (time-to-days (current-time))
           (time-to-days (date-to-time (concat key " 12:00:00"))))
      (error nil))))

(defun vegeta--weeks-ago (key)
  "Return whole weeks from date KEY to the current week, or nil."
  (let ((days (vegeta--days-ago key)))
    (when days (floor days 7))))

(defun vegeta--entry-week-key (entry)
  "Return the Monday (YYYY-MM-DD) of ENTRY's week, or \"unknown\".
Using the Monday date keeps week sections sortable and lets
`vegeta--weeks-ago' work on them directly."
  (let ((key (vegeta--entry-date-key entry)))
    (if (vegeta--date-key-date-p key)
        (let* ((time (date-to-time (concat key " 12:00:00")))
               (dow (string-to-number (format-time-string "%u" time))))
          (format-time-string "%Y-%m-%d" (time-add time (* (- 1 dow) 86400))))
      "unknown")))

(defun vegeta--date-sectioned-p (level)
  "Return non-nil when date LEVEL is active for the current redraw."
  (memq level vegeta--date-levels))

(defun vegeta--date-sublevels (entries)
  "Return the adaptive date sublevels for ENTRIES.
A `date-year' and/or `date-month' level is added only when the entries
span more than one year/month; the leaf is `date-day' or `date-week'
per `vegeta-date-granularity'."
  (let* ((keys (delq nil
                     (mapcar (lambda (e)
                               (let ((k (vegeta--entry-date-key e)))
                                 (and (vegeta--date-key-date-p k) k)))
                             entries)))
         (years (delete-dups (mapcar (lambda (k) (substring k 0 4)) keys)))
         (months (delete-dups (mapcar (lambda (k) (substring k 0 7)) keys))))
    (append (when (> (length years) 1) '(date-year))
            (when (> (length months) 1) '(date-month))
            (list (if (eq vegeta-date-granularity 'weeks)
                      'date-week
                    'date-day)))))

(defun vegeta--expand-date-levels (levels entries)
  "Replace any `date' element of LEVELS with the sublevels for ENTRIES."
  (mapcan (lambda (l)
            (if (eq l 'date) (vegeta--date-sublevels entries) (list l)))
          levels))

(defun vegeta--date-day-label (key)
  "Return the section label for a `date-day' KEY.
Recent days are relative; older days are just the day number, since the
year/month are carried by ancestor sections or the row timestamp."
  (let ((days (vegeta--days-ago key)))
    (cond
     ((null days) (format "%s" key))
     ((= days 0) "Today")
     ((= days 1) "Yesterday")
     ((<= days 6) (format "%d days ago" days))
     (t (substring key 8 10)))))

(defun vegeta--date-week-label (key)
  "Return the section label for a `date-week' KEY."
  (let ((weeks (vegeta--weeks-ago key)))
    (cond
     ((null weeks) (format "%s" key))
     ((= weeks 0) "This week")
     ((= weeks 1) "Last week")
     (t (format "%d weeks ago" weeks)))))

(defun vegeta--group-key (entry level)
  "Return the group key for ENTRY at LEVEL (a symbol)."
  (pcase level
    ('host    (vegeta--entry-host entry))
    ('package (plist-get entry :provider))
    ('repo    (or (plist-get entry :repo) "(no repo)"))
    ('model   (or (vegeta--entry-agent entry) "?"))
    ('date    (vegeta--entry-date-key entry))
    ('date-year (let ((k (vegeta--entry-date-key entry)))
                  (if (vegeta--date-key-date-p k) (substring k 0 4) "unknown")))
    ('date-month (let ((k (vegeta--entry-date-key entry)))
                   (if (vegeta--date-key-date-p k) (substring k 0 7) "unknown")))
    ('date-day (or (vegeta--entry-date-key entry) "unknown"))
    ('date-week (vegeta--entry-week-key entry))
    (_ nil)))

(defun vegeta--provider-levels (provider-id default-levels)
  "Return the tree levels to apply below a `package' node for PROVIDER-ID.
Filters DEFAULT-LEVELS by removing anything listed in the provider's
`:skip-levels'.  Providers use this to avoid degenerate one-child
groupings like `Claude (all)' under `claude-cli' — the model level
adds no information when the provider only ever emits one agent."
  (let* ((prov (alist-get provider-id vegeta-providers))
         (skips (and prov (plist-get prov :skip-levels))))
    (if skips
        (seq-remove (lambda (l) (memq l skips)) default-levels)
      default-levels)))

(defun vegeta--build-tree (entries levels)
  "Group ENTRIES recursively by LEVELS, returning a nested tree.
Node shape: (:level LEVEL :key KEY :breadcrumb (KEYS...)
             :entries LIST :children NODES)."
  (vegeta--build-tree-1 entries levels '()))

(defun vegeta--build-tree-1 (entries levels breadcrumb)
  "See `vegeta--build-tree'.  BREADCRUMB is the path of keys so far."
  (if (null levels)
      entries
    (let* ((level (car levels))
           (rest (cdr levels))
           (groups (make-hash-table :test 'equal))
           (order nil))
      (dolist (e entries)
        (let ((k (vegeta--group-key e level)))
          (unless (gethash k groups)
            (push k order))
          (push e (gethash k groups))))
      (let ((nodes
             (mapcar
              (lambda (k)
                (let* ((es (nreverse (gethash k groups)))
                       (crumb (append breadcrumb (list (cons level k))))
                       ;; Once we've split by `package', each subtree
                       ;; belongs to one provider — switch to that
                       ;; provider's own level list so single-agent
                       ;; providers can collapse the model level.
                       (sub-levels (if (eq level 'package)
                                       (vegeta--provider-levels k rest)
                                     rest)))
                  (list :level level
                        :key k
                        :breadcrumb crumb
                        :entries es
                        :children
                        (vegeta--build-tree-1 es sub-levels crumb))))
              (nreverse order))))
        (sort nodes #'vegeta--node-sort-lessp)))))

(defun vegeta--node-sort-key (node)
  "Return the display-name sort key for NODE, lowercased."
  (downcase
   (format "%s"
           (vegeta--display-name (plist-get node :level)
                                 (plist-get node :key)
                                 (plist-get node :entries)))))

(defun vegeta--host-sort-lessp (a b)
  "Return non-nil when host node A sorts before host node B.
`localhost' always leads; remaining hosts sort alphabetically."
  (let ((ka (plist-get a :key))
        (kb (plist-get b :key)))
    (cond
     ((vegeta--local-host-p ka) (not (vegeta--local-host-p kb)))
     ((vegeta--local-host-p kb) nil)
     (t (string< (vegeta--node-sort-key a) (vegeta--node-sort-key b))))))

(defun vegeta--date-sort-lessp (a b)
  "Return non-nil when date node A sorts before date node B.
Newest first; a non-date key (e.g. \"unknown\") sorts last.  Works for
year (\"2026\"), month (\"2026-09\"), day (\"2026-09-10\") and week
\(\"2026-W37\") keys, which are all lexicographically chronological."
  (let* ((ka (format "%s" (plist-get a :key)))
         (kb (format "%s" (plist-get b :key)))
         (da (string-match-p "\\`[0-9]\\{4\\}" ka))
         (db (string-match-p "\\`[0-9]\\{4\\}" kb)))
    (cond
     ((and da db) (string> ka kb))
     (da t)
     (db nil)
     (t (string< ka kb)))))

(defun vegeta--node-sort-lessp (a b)
  "Return non-nil when node A sorts before sibling node B.
Sections are alphabetical, with two exceptions: `localhost' leads the
`host' level, and date sections are newest first."
  (pcase (plist-get a :level)
    ('host (vegeta--host-sort-lessp a b))
    ((or 'date 'date-year 'date-month 'date-day 'date-week)
     (vegeta--date-sort-lessp a b))
    (_     (string< (vegeta--node-sort-key a) (vegeta--node-sort-key b)))))

;;; Rendering

(defconst vegeta--placeholder
  (propertize "…" 'face 'vegeta-placeholder-face))

(defun vegeta--display-name (level key entries)
  "Return the display string for a group header at LEVEL with KEY."
  (pcase level
    ('host    (or key vegeta--local-host))
    ('package
     (let ((p (alist-get key vegeta-providers)))
       (or (and p (plist-get p :name)) (symbol-name key))))
    ('repo
     (let* ((roots (delete-dups
                    (delq nil
                          (mapcar (lambda (e) (plist-get e :repo)) entries))))
            (all-roots (or roots (list key)))
            (names (vegeta--disambiguated-names all-roots)))
       (or (gethash key names)
           (file-name-nondirectory (directory-file-name key)))))
    ('model (or key "?"))
    ('date (or key "unknown"))
    ('date-year (if (equal key (format-time-string "%Y"))
                    "This year"
                  (format "%s" key)))
    ('date-month
     (if (equal key (format-time-string "%Y-%m"))
         "This month"
       (condition-case nil
           (format-time-string "%B" (date-to-time (concat key "-01 12:00:00")))
         (error (format "%s" key)))))
    ('date-day (vegeta--date-day-label key))
    ('date-week (vegeta--date-week-label key))
    (_ (format "%s" key))))

(defun vegeta--face-for-level (level)
  "Return the face used for a header at LEVEL."
  (pcase level
    ('host    'vegeta-host-face)
    ('package 'vegeta-package-face)
    ('repo    'vegeta-project-face)
    ('model   'vegeta-agent-face)
    ((or 'date 'date-year 'date-month 'date-day 'date-week) 'vegeta-date-face)
    (_        'vegeta-project-face)))

(defun vegeta--effective-levels (levels)
  "Return LEVELS with `host' dropped when only localhost is configured.
Keeps existing single-machine setups from growing a lone `localhost'
node; the level reappears as soon as a remote host is added."
  (if (vegeta--remote-hosts)
      levels
    (seq-remove (lambda (l) (eq l 'host)) levels)))

(defun vegeta--render-group-header (node depth entries-at-node)
  "Insert a group header line for NODE at DEPTH.
ENTRIES-AT-NODE is the flat list of entries below this node."
  (let* ((level (plist-get node :level))
         (key (plist-get node :key))
         (crumb (plist-get node :breadcrumb))
         (collapsed (gethash crumb vegeta--collapsed))
         (arrow (if collapsed "▸" "▾"))
         (name (vegeta--display-name level key entries-at-node))
         (indent (make-string (* 2 depth) ?\s))
         (label (format "%s%s %s (%d)"
                        indent arrow name (length entries-at-node)))
         (start (point)))
    (insert (propertize label 'face (vegeta--face-for-level level)))
    (add-text-properties
     start (point)
     `(vegeta-group ,crumb
       vegeta-collapsed ,collapsed))
    (insert "\n")))

(defun vegeta--window-for-buffer ()
  "Return a window displaying the current buffer, or nil.
Prefers the selected window so truncation follows the window the user
is actually looking at when the buffer is shown in more than one."
  (or (and (eq (window-buffer (selected-window)) (current-buffer))
           (selected-window))
      (get-buffer-window (current-buffer) t)
      (car (get-buffer-window-list (current-buffer) nil t))))

(defun vegeta--max-line-width ()
  "Resolve `vegeta-max-line-width' to a column count, or nil for no limit.
An integer is used as-is.  The symbol `window' yields the body width of
a window displaying the current buffer, preferring the selected window
and falling back to the selected window's width when the buffer isn't
displayed yet (e.g. while it is being created and first refreshed).
Returns nil to disable truncation."
  (cond
   ((integerp vegeta-max-line-width) vegeta-max-line-width)
   ((eq vegeta-max-line-width 'window)
    (window-body-width (or (vegeta--window-for-buffer)
                           (selected-window))))
   (t nil)))

(defun vegeta--truncate-title (title prefix-width)
  "Truncate TITLE so PREFIX-WIDTH + its length fits the row width.
The row width comes from `vegeta--max-line-width', which resolves
`vegeta-max-line-width' (a fixed integer, the displaying window's body
width, or nil for no limit).  Returns TITLE unchanged when the limit is
disabled or the title already fits.  Uses `truncate-string-to-width'
with an ellipsis so multi-byte characters are counted correctly."
  (let ((max-width (vegeta--max-line-width)))
    (if (or (null title)
            (null max-width)
            (<= (+ prefix-width (string-width title)) max-width))
        title
      (truncate-string-to-width
       title (max 1 (- max-width prefix-width))
       nil nil "…"))))

(defun vegeta--render-entry-row (entry depth mark)
  "Insert one chat row for ENTRY at DEPTH with optional MARK."
  (let* ((parsed (not (null (vegeta--cached-meta entry))))
         (date-str (vegeta--entry-timestamp entry))
         (date (if parsed
                   (propertize date-str 'face 'vegeta-date-face)
                 (propertize date-str
                             'face 'vegeta-placeholder-face)))
         (title (vegeta--entry-title entry))
         (started (vegeta--entry-started-at-seconds entry))
         (updated (vegeta--entry-updated-at-seconds entry))
         (updated-suffix
          (and vegeta-updated-suffix-min-delta
               started updated
               (> (- updated started) vegeta-updated-suffix-min-delta)
               (propertize
                (format-time-string
                 (concat " (updated %m-%d " vegeta-time-format ")")
                 (seconds-to-time updated))
                'face 'vegeta-updated-face)))
         (mark-str (vegeta--mark-string mark))
         (indent (make-string (+ 2 (* 2 depth)) ?\s))
         ;; Everything except the title itself contributes to the
         ;; prefix width used by `vegeta--truncate-title'.  Include
         ;; the `: ' separator we add before the title below.
         (prefix-width (+ (length indent) 1 1
                          (string-width date-str)
                          (if updated-suffix
                              (string-width updated-suffix) 0)
                          2))
         (title (vegeta--truncate-title title prefix-width))
         (start (point)))
    (insert indent mark-str " " date
            (or updated-suffix "")
            (cond
             (title (concat ": " title))
             (parsed "")
             (t (concat ": " vegeta--placeholder))))
    (add-text-properties
     start (point)
     `(vegeta-entry ,entry
       help-echo ,(vegeta--entry-help entry)))
    (insert "\n")))

(defun vegeta--entry-help (entry)
  "Return a tooltip string for ENTRY."
  (let ((meta (vegeta--cached-meta entry))
        (id (plist-get entry :id)))
    (if meta
        (let ((renamed (plist-get meta :renamed))
              (ai-title (plist-get meta :ai-title))
              (first-prompt (plist-get meta :first-prompt)))
          (format "%s\nHost: %s\nProvider: %s\nAgent: %s\nSession: %s\nCwd: %s%s%s%s"
                  id
                  (vegeta--entry-host entry)
                  (plist-get entry :provider)
                  (or (plist-get meta :agent) "?")
                  (or (plist-get meta :session-id) "(none)")
                  (or (plist-get meta :cwd) "?")
                  (if renamed (format "\nRenamed: %s" renamed) "")
                  (if ai-title (format "\nAI title: %s" ai-title) "")
                  (if first-prompt (format "\n\n%s" first-prompt) "")))
      id)))

(defun vegeta--render-tree (nodes depth)
  "Render NODES at DEPTH recursively into the current buffer."
  (dolist (node nodes)
    (let* ((entries (plist-get node :entries))
           (children (plist-get node :children))
           (crumb (plist-get node :breadcrumb))
           (collapsed (gethash crumb vegeta--collapsed)))
      (vegeta--render-group-header node depth entries)
      (unless collapsed
        (cond
         ;; children is a list of nodes (deeper grouping): recurse
         ((and (consp children)
               (consp (car children))
               (plist-get (car children) :level))
          (vegeta--render-tree children (1+ depth)))
         ;; children is a list of entries (leaf level): render each
         (t
          (dolist (e children)
            (vegeta--render-entry-row
             e (1+ depth)
             (gethash (plist-get e :id) vegeta--marks)))))))))

(defun vegeta--redraw ()
  "Redraw the current sidebar buffer, preserving point when possible."
  (when (derived-mode-p 'vegeta-mode)
    (setq vegeta--rendered-width (vegeta--max-line-width))
    (let* ((prev-entry-id
            (let ((e (get-text-property (point) 'vegeta-entry)))
              (and e (plist-get e :id))))
           (prev-group (get-text-property (point) 'vegeta-group))
           (prev-line (line-number-at-pos))
           (inhibit-read-only t)
           (entries (vegeta--all-entries)))
      (erase-buffer)
      (unless vegeta--marks
        (setq vegeta--marks (make-hash-table :test 'equal)))
      (unless vegeta--collapsed
        (setq vegeta--collapsed (make-hash-table :test 'equal)))
      (cond
       ((null entries)
        (insert (propertize "  (no chats found)\n"
                            'face 'vegeta-placeholder-face)))
       ((null vegeta-grouping)
        (let ((sorted (sort (copy-sequence entries)
                            (lambda (a b)
                              (let ((ma (plist-get a :mtime))
                                    (mb (plist-get b :mtime)))
                                (cond
                                 ((and (null ma) (null mb)) nil)
                                 ((null ma) nil)
                                 ((null mb) t)
                                 (t (> ma mb))))))))
          (dolist (e sorted)
            (vegeta--render-entry-row
             e 0 (gethash (plist-get e :id) vegeta--marks)))))
       (t
        (let* ((levels (vegeta--effective-levels vegeta-grouping))
               (date-p (memq 'date levels)))
          (setq vegeta--date-levels
                (and date-p (vegeta--date-sublevels entries)))
          (vegeta--render-tree
           (vegeta--build-tree
            entries
            (if date-p (vegeta--expand-date-levels levels entries) levels))
           0))))
      (goto-char (point-min))
      (cond
       (prev-entry-id
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (let ((e (get-text-property (point) 'vegeta-entry)))
              (when (and e (equal prev-entry-id (plist-get e :id)))
                (setq found t)))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min))
                  (forward-line (1- prev-line)))))
       (prev-group
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (equal prev-group
                         (get-text-property (point) 'vegeta-group))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found (goto-char (point-min)))))
       (t (forward-line (1- prev-line)))))))

(defun vegeta--window-size-changed (&optional frame)
  "Redraw vegeta buffers in FRAME whose displaying window width changed.
Registered on `window-size-change-functions' so resizing the sidebar or
the full-frame view re-fits titles to the new width.  Buffers already
rendered at the new width are left untouched."
  (dolist (win (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer win)
      (when (and (derived-mode-p 'vegeta-mode)
                 (not (equal (window-body-width win)
                             vegeta--rendered-width)))
        (vegeta--redraw)))))

(add-hook 'window-size-change-functions #'vegeta--window-size-changed)

;;; Sidebar mode + keymap

(defvar vegeta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vegeta-visit)
    (define-key map (kbd "o") #'vegeta-open-transcript)
    (define-key map (kbd "g") #'vegeta-refresh)
    (define-key map (kbd "s") #'vegeta-search)
    (define-key map (kbd "q") #'vegeta-hide-sidebar)
    (define-key map (kbd "n") #'vegeta-next-line)
    (define-key map (kbd "p") #'vegeta-previous-line)
    (define-key map (kbd "d") #'vegeta-mark-delete)
    (define-key map (kbd "m") #'vegeta-mark)
    (define-key map (kbd "u") #'vegeta-unmark)
    (define-key map (kbd "U") #'vegeta-unmark-all)
    (define-key map (kbd "x") #'vegeta-execute)
    (define-key map (kbd "r") #'vegeta-rename)
    (define-key map (kbd "TAB") #'vegeta-toggle-group)
    (define-key map (kbd "<tab>") #'vegeta-toggle-group)
    (define-key map [mouse-2] #'vegeta-mouse-visit)
    map)
  "Keymap for `vegeta-mode'.")

(define-derived-mode vegeta-mode special-mode "AgentChats"
  "Major mode for the AI agent chat sidebar."
  :group 'vegeta
  (setq truncate-lines t
        buffer-read-only t
        window-size-fixed vegeta-window-fixed
        cursor-in-non-selected-windows nil)
  (setq-local vegeta--marks (make-hash-table :test 'equal))
  (setq-local vegeta--collapsed (make-hash-table :test 'equal))
  (when vegeta-refresh-timer
    (setq-local vegeta--refresh-timer-object
                (run-with-idle-timer
                 vegeta-refresh-timer t
                 (lambda ()
                   (when (buffer-live-p (get-buffer vegeta-name))
                     (with-current-buffer (get-buffer vegeta-name)
                       (vegeta-refresh)))))))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (timerp vegeta--refresh-timer-object)
                (cancel-timer vegeta--refresh-timer-object)))
            nil t))

;;; Commands: navigation

(defun vegeta-next-line (&optional n)
  "Move to the next actionable row (entry or group header)."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (forward-line (if (> n 0) 1 -1))
      (while (and (not (eobp))
                  (not (bobp))
                  (not (get-text-property (point) 'vegeta-entry))
                  (not (get-text-property (point) 'vegeta-group)))
        (forward-line (if (> n 0) 1 -1))))))

(defun vegeta-previous-line (&optional n)
  "Move to the previous actionable row."
  (interactive "p")
  (vegeta-next-line (- (or n 1))))

;;; Commands: activation

(defun vegeta--pop-to (buffer)
  "Pop to BUFFER in the MRU or next window, per user setting."
  (let ((win (if vegeta-open-file-in-most-recently-used-window
                 (get-mru-window nil nil t)
               (next-window))))
    (if win
        (progn (select-window win)
               (switch-to-buffer buffer))
      (pop-to-buffer buffer))))

(defun vegeta-visit (&optional prefix)
  "Activate the row at point.
Group headers toggle their fold.  Chat rows dispatch to the entry's
provider `:visit' function by default; with PREFIX arg (\\[universal-argument])
the raw transcript file is opened instead."
  (interactive "P")
  (cond
   ((get-text-property (point) 'vegeta-group)
    (vegeta-toggle-group))
   ((get-text-property (point) 'vegeta-entry)
    (if prefix
        (vegeta-open-transcript)
      (let* ((entry (get-text-property (point) 'vegeta-entry))
             (provider (vegeta--entry-provider entry))
             (visitor (plist-get provider :visit)))
        (unless visitor
          (user-error "Provider %s has no :visit function"
                      (plist-get entry :provider)))
        (vegeta--call-with-host provider (vegeta--entry-host entry)
                                visitor entry))))
   (t (user-error "Nothing at point"))))

(defun vegeta-mouse-visit (event)
  "Handle mouse click EVENT on a row."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (vegeta-visit))))

(defun vegeta-open-transcript ()
  "Open the source for the entry at point (read-only view).
Providers may supply an `:open-transcript' function (ENTRY -> buffer) to
render a friendlier transcript; when absent, the entry's `:id' file is
opened directly."
  (interactive)
  (let ((entry (get-text-property (point) 'vegeta-entry)))
    (unless entry (user-error "No entry at point"))
    (let* ((provider (vegeta--entry-provider entry))
           (opener (and provider (plist-get provider :open-transcript))))
      (vegeta--pop-to (if opener
                          (funcall opener entry)
                        (find-file-noselect (plist-get entry :id)))))))

(defun vegeta-toggle-group ()
  "Fold or unfold the group at point."
  (interactive)
  (let ((crumb (get-text-property (point) 'vegeta-group)))
    (unless crumb (user-error "Not on a group header"))
    (if (gethash crumb vegeta--collapsed)
        (remhash crumb vegeta--collapsed)
      (puthash crumb t vegeta--collapsed))
    (vegeta--redraw)))

;;; Commands: marks

(defun vegeta--mark-string (mark)
  "Return the display string for MARK (a symbol), or a blank column.
Dired-style: `*' for the generic mark, `D' for the deletion flag."
  (pcase mark
    ('delete (propertize "D" 'face 'vegeta-flag-face))
    ('marked (propertize "*" 'face 'vegeta-mark-face))
    (_ " ")))

(defun vegeta--set-mark (mark)
  "Set MARK (or clear it when nil) on the entry at point, then move down."
  (let ((entry (get-text-property (point) 'vegeta-entry)))
    (unless entry (user-error "No entry at point"))
    (if mark
        (puthash (plist-get entry :id) mark vegeta--marks)
      (remhash (plist-get entry :id) vegeta--marks))
    (vegeta--redraw)
    (vegeta-next-line 1)))

(defun vegeta-mark ()
  "Mark the entry at point with `*' (dired `m'), then move down."
  (interactive)
  (vegeta--set-mark 'marked))

(defun vegeta-mark-delete ()
  "Flag the entry at point for deletion with `D' (dired `d'), then move down.
Only `D'-flagged entries are removed by `vegeta-execute'."
  (interactive)
  (vegeta--set-mark 'delete))

(defun vegeta-unmark ()
  "Remove any mark from the entry at point, then move down (dired `u')."
  (interactive)
  (vegeta--set-mark nil))

(defun vegeta-unmark-all ()
  "Remove every mark in the buffer (dired `U')."
  (interactive)
  (clrhash vegeta--marks)
  (vegeta--redraw))

;;; Commands: rename

(defun vegeta-rename (new-title)
  "Set a user-supplied title for the entry at point.
Stored as `:renamed' in the entry's cached metadata; empty input clears
the override so the row falls back to `:ai-title' or `:first-prompt'."
  (interactive
   (list (let* ((entry (or (get-text-property (point) 'vegeta-entry)
                           (user-error "No entry at point")))
                (meta (vegeta--cached-meta entry)))
           (unless meta (user-error "Entry not parsed yet; press g to refresh"))
           (read-string "Rename to (empty to clear): "
                        (or (plist-get meta :renamed)
                            (plist-get meta :ai-title)
                            (plist-get meta :first-prompt)
                            "")))))
  (let* ((entry (get-text-property (point) 'vegeta-entry))
         (meta (vegeta--cached-meta entry))
         (trimmed (string-trim new-title))
         (new-meta (plist-put (copy-sequence meta)
                              :renamed
                              (if (string-empty-p trimmed) nil trimmed))))
    (vegeta--put-cache entry new-meta)
    (vegeta--redraw)))

(defun vegeta--entry-by-id (id)
  "Return the discovered entry with `:id' equal to ID, or nil.
Consults `vegeta--entries-cache' — populated by the most recent
refresh — so this is O(n) in the entry count but doesn't re-scan the
filesystem."
  (seq-find (lambda (e) (equal id (plist-get e :id)))
            (or vegeta--entries-cache (vegeta--all-entries))))

(defun vegeta--confirm-delete (n)
  "Return non-nil when the user confirms deleting N chats.
Always asks once, naming the count; when N exceeds
`vegeta-delete-double-confirm-threshold' (and it is non-nil) a second,
stronger confirmation follows."
  (and (yes-or-no-p (format "About to delete %d chat(s).  Are you sure? " n))
       (or (null vegeta-delete-double-confirm-threshold)
           (<= n vegeta-delete-double-confirm-threshold)
           (yes-or-no-p (format "Really delete %d chats?  This cannot be undone. " n)))))

(defun vegeta-execute ()
  "Delete all entries marked for deletion, after confirmation.
Each entry's `:delete' provider hook decides how to remove its
storage — most providers just delete the file, but a Claude CLI
session also cleans its sidecar `session-env/<uuid>' directory.
Failures on individual entries are reported but don't abort the batch.
Deletion always asks for confirmation, naming the count; batches larger
than `vegeta-delete-double-confirm-threshold' ask a second time."
  (interactive)
  (let (ids)
    (maphash (lambda (id mark)
               (when (eq mark 'delete) (push id ids)))
             vegeta--marks)
    (cond
     ((null ids) (message "No marks to execute"))
     ((not (vegeta--confirm-delete (length ids)))
      (message "Not deleting %d chat(s)" (length ids)))
     (t
      (let ((ok 0) (fail 0))
        (dolist (id ids)
          (let* ((entry (vegeta--entry-by-id id))
                 (provider (and entry (vegeta--entry-provider entry)))
                 (deleter (or (and provider (plist-get provider :delete))
                              #'vegeta--default-delete)))
            (condition-case err
                (progn
                  (when entry
                    (vegeta--call-with-host provider
                                            (vegeta--entry-host entry)
                                            deleter entry))
                  (cl-incf ok))
              (error
               (cl-incf fail)
               (message "vegeta: failed to delete %s: %S"
                        (file-name-nondirectory id)
                        (error-message-string err)))))
          (remhash id vegeta--parse-cache)
          (remhash id vegeta--marks))
        (vegeta--cache-mark-dirty)
        (vegeta-refresh)
        (message "vegeta: deleted %d chat(s)%s"
                 ok (if (zerop fail) "" (format " (%d failed)" fail))))))))

;;; Commands: refresh

(defun vegeta-refresh ()
  "Rescan providers and redraw.
On the first invocation of an Emacs session, the persisted cache from
`vegeta-cache-file' is loaded before the scan so entries with fresh
cached metadata render immediately.

Local hosts are listed synchronously (fast).  Remote `vegeta-hosts'
machines are listed in background subprocesses so a slow or unreachable
host cannot freeze Emacs; their entries merge in and redraw as they
arrive.  Uncached entries are parsed in subprocess workers when the
`async' library is installed and `vegeta-async-workers' > 0; otherwise
they fall through to the in-process idle-timer parser."
  (interactive)
  (unless vegeta--cache-loaded
    (vegeta--cache-load))
  (setq vegeta--entries-cache nil)
  (clrhash vegeta--parse-queued-ids)
  (setq vegeta--parse-queue nil)
  (let ((local (vegeta--local-host-entries)))
    (setq vegeta--entries-cache local)
    (vegeta--queue-uncached local)
    (vegeta--redraw))
  (if (vegeta--async-available-p)
      (vegeta--start-host-jobs)
    (progn (vegeta--start-host-jobs-sync)
           (vegeta--redraw)))
  (vegeta--maybe-start-parse))

;;; Commands: search

(defcustom vegeta-search-scope 'content
  "Default sections searched by `vegeta-search'.
One of:
  `content'  — user and agent prose (default; skips tool-call noise)
  `user'     — user messages only
  `agent'    — agent replies only
  `thoughts' — the above plus the agent's reasoning
  `all'      — raw, including tool calls and their output
Providers translate this to their own storage."
  :type '(choice (const content) (const user) (const agent)
                 (const thoughts) (const all))
  :group 'vegeta)

(defun vegeta--read-scope ()
  "Prompt for a `vegeta-search' scope and return it as a symbol."
  (intern (completing-read "Search scope: "
                           '("content" "user" "agent" "thoughts" "all")
                           nil t nil nil "content")))

(defun vegeta--read-host ()
  "Prompt for one of `vegeta-hosts'; default to the first."
  (let ((hosts (mapcar #'car (vegeta--hosts))))
    (completing-read "Host: " hosts nil t nil nil (car hosts))))

(defun vegeta--search-visit (host cand)
  "Open CAND, a FILE:LINE:TEXT search match from HOST.
FILE is host-native, so it is tramp-qualified for a remote HOST.  When
FILE maps back to a known entry whose provider offers `:open-search-result'
\(or `:open-transcript') — e.g. ECA, whose storage is opaque Transit
rather than a line-addressable file — that renderer is used, so the match
opens the readable chat instead of the raw source.  Otherwise the file is
opened at the match's line."
  (if (string-match "\\`\\(.+?\\):\\([0-9]+\\):" cand)
      (let* ((file (vegeta--hostify host (match-string 1 cand)))
             (line (string-to-number (match-string 2 cand)))
             (entry (vegeta--entry-by-id file))
             (provider (and entry (vegeta--entry-provider entry)))
             (opener (and provider (or (plist-get provider :open-search-result)
                                       (plist-get provider :open-transcript)))))
        (if opener
            (vegeta--pop-to (funcall opener entry))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (forward-line (1- (max 1 line)))
            (recenter))))
    (user-error "Not a FILE:LINE match: %S" cand)))

(defun vegeta--search-directory (host)
  "Return the fzfa `:directory' that targets HOST.
A local directory for localhost; the Tramp root (`/ssh:HOST:/') for a
remote host, so `fzfa-tramp' ssh-wraps the command on that machine."
  (if (vegeta--local-host-p host)
      default-directory
    (concat (vegeta--host-tramp-prefix host) "/")))

(defun vegeta--search-source (host provider scope targets)
  "Build an fzfa source for PROVIDER on HOST, or nil.
TARGETS, when non-nil, restricts the search to those native paths (the
marked chats); nil lets the provider search its default roots.  The
source carries its own `:action' so a multi-source selection visits the
right host."
  (when (plist-get provider :search-command)
    (when-let* ((cmd (funcall (plist-get provider :search-command)
                              host scope targets)))
      (list :name (format "%s · %s" host
                          (or (plist-get provider :name)
                              (symbol-name (plist-get provider :id))))
            :command cmd
            :directory (vegeta--search-directory host)
            :action (lambda (cand) (vegeta--search-visit host cand))))))

(defun vegeta--marked-entries ()
  "Return the entries currently marked in this buffer, or nil."
  (when (and (hash-table-p vegeta--marks)
             (> (hash-table-count vegeta--marks) 0))
    (seq-filter (lambda (e) (gethash (plist-get e :id) vegeta--marks))
                (or vegeta--entries-cache (vegeta--all-entries)))))

(defun vegeta--search-sources-for-host (host scope)
  "Return fzfa sources for every searchable provider on HOST."
  (let ((spec (cdr (assoc host (vegeta--hosts)))))
    (delq nil
          (mapcar (lambda (p) (vegeta--search-source host p scope nil))
                  (vegeta--providers-for-host (or spec 'all))))))

(defun vegeta--search-sources-for-marked (entries scope)
  "Return fzfa sources for marked ENTRIES, one per (host, provider).
Each source searches only its own marked files, so marks spread across
hosts or providers become independent `fzfa' sources."
  (let ((groups (make-hash-table :test 'equal))
        (order nil))
    (dolist (e entries)
      (let ((key (cons (vegeta--entry-host e) (plist-get e :provider))))
        (unless (gethash key groups) (push key order))
        (puthash key (cons e (gethash key groups)) groups)))
    (delq nil
          (mapcar
           (lambda (key)
             (let* ((host (car key))
                    (provider (alist-get (cdr key) vegeta-providers))
                    (targets (mapcar (lambda (e)
                                       (vegeta--unhostify host (plist-get e :id)))
                                     (nreverse (gethash key groups)))))
               (vegeta--search-source host provider scope targets)))
           (nreverse order)))))

(defun vegeta--run-search (sources scope)
  "Run fzfa over SOURCES; a single source inline, several via multi-read."
  (if (null (cdr sources))
      (let ((s (car sources)))
        (when-let* ((cand (fzfa-completing-read
                           :prompt (format "Search [%s]: " scope)
                           :command (plist-get s :command)
                           :directory (plist-get s :directory)
                           :category 'fzfa-grep
                           :skip-executable-check t)))
          (funcall (plist-get s :action) cand)))
    (when (fboundp 'fzfa--ensure-setup) (fzfa--ensure-setup))
    (when (fboundp 'fzfa--multi-allocate-narrow-keys)
      (setq sources (fzfa--multi-allocate-narrow-keys sources)))
    (fzfa--read sources :prompt (format "Search [%s]: " scope))))

;;;###autoload
(defun vegeta-search (&optional host scope)
  "Fuzzy full-text search across chat transcripts.
With no entries marked, searches HOST's providers over their default
roots, prompting for HOST when it is nil.  With entries marked, searches
only those marked chats — one `fzfa' source per (host, provider), so
marks spread across machines or providers stay separate.  SCOPE
\(default `vegeta-search-scope') selects which sections are searched;
with a prefix argument, prompt for it."
  (interactive
   (list nil (when current-prefix-arg (vegeta--read-scope))))
  (unless (require 'fzfa nil t)
    (user-error "vegeta-search requires the `fzfa' package"))
  (let* ((scope (or scope vegeta-search-scope))
         (marked (vegeta--marked-entries))
         (sources (if marked
                      (vegeta--search-sources-for-marked marked scope)
                    (vegeta--search-sources-for-host
                     (or host (vegeta--read-host)) scope))))
    (when (null sources)
      (user-error "Nothing to search%s"
                  (if marked " among the marked chats" "")))
    (vegeta--run-search sources scope)))

;;;###autoload
(defun vegeta-search-user (&optional host)
  "Like `vegeta-search', but only match user messages."
  (interactive)
  (vegeta-search host 'user))

;;; Sidebar window commands

(defun vegeta--sidebar-window (&optional _frame)
  "Return the sidebar window if visible in the current frame."
  (seq-find
   (lambda (w)
     (with-current-buffer (window-buffer w)
       (derived-mode-p 'vegeta-mode)))
   (window-list)))

(defun vegeta-showing-sidebar-p ()
  "Non-nil if the sidebar is visible in the selected frame."
  (vegeta--sidebar-window))

(defun vegeta--get-or-create-buffer (&optional name fixed)
  "Return a vegeta buffer, creating and initializing it if needed.
NAME defaults to `vegeta-name' (the sidebar buffer).  FIXED overrides
`window-size-fixed' in the newly-created buffer — pass nil to allow
free resizing (useful for the full-frame view), or a symbol like
`width' to keep the sidebar's fixed sizing."
  (let* ((name (or name vegeta-name))
         (existing (get-buffer name)))
    (or existing
        (let ((buf (generate-new-buffer name)))
          (with-current-buffer buf
            (vegeta-mode)
            (setq-local window-size-fixed fixed)
            (vegeta-refresh))
          buf))))

(defun vegeta--set-width (width)
  "Set the sidebar window width to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

;;;###autoload
(defun vegeta ()
  "Open the agent chat browser in the current window.
Unlike `vegeta-show-sidebar', which docks the browser in a
narrow side-window, this fills the current window with the browser
buffer — handy for a full-frame view where the tree, dates, and
titles all get room to breathe.

Uses a separate buffer (`vegeta-buffer-name') from the sidebar, so
the two can coexist.  Both share the same on-disk cache and provider
registry, so parsing done in one is immediately visible in the other."
  (interactive)
  (switch-to-buffer
   (vegeta--get-or-create-buffer vegeta-buffer-name nil)))

;;;###autoload
(defun vegeta-show-sidebar ()
  "Show the agent chat sidebar."
  (interactive)
  (let ((buffer (vegeta--get-or-create-buffer vegeta-name vegeta-window-fixed)))
    (display-buffer-in-side-window buffer vegeta-display-alist)
    (let ((window (get-buffer-window buffer)))
      (when window
        (set-window-dedicated-p window t)
        (when vegeta-no-delete-other-windows
          (set-window-parameter window 'no-delete-other-windows t))
        (when vegeta-resize-on-open
          (with-selected-window window
            (let ((window-size-fixed))
              (vegeta--set-width vegeta-width))))))))

;;;###autoload
(defun vegeta-hide-sidebar ()
  "Hide the agent chat sidebar in the selected frame."
  (interactive)
  (when-let* ((win (vegeta--sidebar-window)))
    (delete-window win)))

;;;###autoload
(defun vegeta-toggle-sidebar ()
  "Toggle the agent chat sidebar."
  (interactive)
  (if (vegeta-showing-sidebar-p)
      (vegeta-hide-sidebar)
    (vegeta-show-sidebar)
    (when vegeta-pop-to-sidebar-on-toggle-open
      (when-let* ((win (vegeta--sidebar-window)))
        (select-window win)))))

;;;###autoload
(defun vegeta-jump-to-sidebar ()
  "Jump to the sidebar window, showing it first if hidden."
  (interactive)
  (if-let* ((win (vegeta--sidebar-window)))
      (select-window win)
    (call-interactively #'vegeta-toggle-sidebar)))

;;; Evil bindings

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal vegeta-mode-map
      (kbd "RET") #'vegeta-visit
      (kbd "o")   #'vegeta-open-transcript
      (kbd "gr")  #'vegeta-refresh
      (kbd "s")   #'vegeta-search
      (kbd "gg")  #'evil-goto-first-line
      (kbd "G")   #'evil-goto-line
      (kbd "j")   #'vegeta-next-line
      (kbd "k")   #'vegeta-previous-line
      (kbd "d")   #'vegeta-mark-delete
      (kbd "m")   #'vegeta-mark
      (kbd "u")   #'vegeta-unmark
      (kbd "U")   #'vegeta-unmark-all
      (kbd "x")   #'vegeta-execute
      (kbd "r")   #'vegeta-rename
      (kbd "TAB") #'vegeta-toggle-group
      (kbd "^")   #'vegeta-toggle-group
      (kbd "-")   #'vegeta-toggle-group
      (kbd "q")   #'vegeta-hide-sidebar
      (kbd "ZZ")  #'vegeta-hide-sidebar
      (kbd "ZQ")  #'vegeta-hide-sidebar))
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map vegeta-mode-map 'normal)))

(provide 'vegeta-core)
;;; vegeta-core.el ends here
