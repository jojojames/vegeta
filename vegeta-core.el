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

(defcustom vegeta-name "*:AgentChats:*"
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
Merged with `project-known-project-roots' and
`projectile-known-projects'."
  :type '(repeat directory))

(defcustom vegeta-collapse-empty-projects t
  "Hide projects with no entries from the sidebar."
  :type 'boolean)

(defcustom vegeta-grouping '(package repo model date)
  "Grouping levels for the sidebar tree, outermost first.
Each element is one of the recognized levels:
  `package' — group by provider (e.g. agent-shell, Claude CLI)
  `repo'    — group by project/repository root
  `model'   — group by agent/model name (Claude, Codex, ...)
  `date'    — group by YYYY-MM-DD (from parsed timestamp or file mtime)

An empty list produces a flat listing sorted by date."
  :type '(repeat (choice (const package)
                         (const repo)
                         (const model)
                         (const date))))

(defcustom vegeta-enabled-providers '(agent-shell claude-cli)
  "Provider ids to include in the sidebar.
See `vegeta-providers' for available providers."
  :type '(repeat symbol))

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
  '((t :inherit warning))
  "Face for row marks (e.g. delete).")

(defface vegeta-placeholder-face
  '((t :inherit shadow))
  "Face for un-parsed placeholder metadata.")

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

(defvar vegeta--async-pending 0
  "Number of async worker batches still in flight.
When it hits zero after a refresh, the persisted cache flushes to disk.")

(declare-function async-start "async")

(defvar-local vegeta--marks nil
  "Hash table mapping entry id -> mark symbol (e.g. `delete').")

(defvar-local vegeta--collapsed nil
  "Hash table of collapsed group breadcrumbs (list of level keys).")

(defvar-local vegeta--refresh-timer-object nil
  "Per-buffer idle timer for auto-refresh.")

(defvar vegeta--cache-loaded nil
  "Non-nil once the on-disk cache has been loaded (or attempted).")

(defvar vegeta--cache-dirty nil
  "Non-nil when in-memory cache has changes not yet flushed to disk.")

(defvar vegeta--cache-flush-timer nil
  "Active idle timer for flushing the cache to disk.")

;;; Project roots

(defun vegeta--project-roots ()
  "Return the union of known project roots, deduplicated and normalized."
  (let ((roots (append
                (when (fboundp 'project-known-project-roots)
                  (project-known-project-roots))
                (when (bound-and-true-p projectile-known-projects)
                  projectile-known-projects)
                vegeta-extra-project-roots)))
    (thread-last roots
                 (mapcar (lambda (r)
                           (when r
                             (expand-file-name (file-name-as-directory r)))))
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

;;; Time utilities

(defun vegeta--iso-to-seconds (str)
  "Parse timestamp STR to seconds-since-epoch, or nil.
Accepts both agent-shell's local-time format and Claude's ISO UTC form."
  (condition-case _
      (float-time (date-to-time str))
    (error nil)))

;;; Terminal launcher

(defun vegeta--launch-terminal (name program args directory)
  "Launch PROGRAM ARGS in a terminal buffer named NAME at DIRECTORY."
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

;;; Provider protocol
;;
;; A provider is a plist with the keys:
;;   :id       symbol, e.g. `agent-shell'                          (required)
;;   :name     display string, e.g. "agent-shell"                  (required)
;;   :list     () -> list of ENTRY plists (metadata may be partial) (required)
;;   :parse    (ENTRY) -> META plist                               (required)
;;   :visit    (ENTRY) -> side-effect (opens/resumes)              (required)
;;   :date-key (ENTRY) -> "YYYY-MM-DD" for date grouping           (optional)
;;
;; An ENTRY is a plist with:
;;   :provider SYMBOL             (provider id)
;;   :id       STRING             (globally unique; usually the source file path)
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
             (parser (plist-get provider :parse))
             (meta (and parser (funcall parser entry))))
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
  "Idle-timer tick: flush cache to disk when dirty and parsing has drained."
  (when (and vegeta--cache-dirty
             (null vegeta--parse-queue))
    (vegeta--cache-save))
  (unless vegeta--cache-dirty
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

(defun vegeta--async-worker-form (batch lib-dir)
  "Return the lambda form the async worker should evaluate.
BATCH is the entry list to parse; LIB-DIR is the directory containing
`vegeta-core.el' so the worker can add it to its load-path."
  `(lambda ()
     ;; The worker inherits none of the parent's state.  Load just
     ;; enough of vegeta to register providers and reach the parsers;
     ;; we deliberately avoid `package-initialize' since parsing needs
     ;; no ELPA packages (agent-shell is lazy-required by :visit).
     (setq load-path (cons ,lib-dir load-path))
     (require 'vegeta-core)
     (require 'vegeta-claude-cli)
     (require 'vegeta-agent-shell)
     (mapcar
      (lambda (entry)
        (let* ((prov (alist-get (plist-get entry :provider)
                                vegeta-providers))
               (parser (plist-get prov :parse)))
          ;; Return a compact tuple; the caller writes it into
          ;; `vegeta--parse-cache' with the current schema version.
          (list (plist-get entry :id)
                (plist-get entry :mtime)
                (and parser (funcall parser entry)))))
      ',batch)))

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

(defun vegeta--start-async-parse ()
  "Dispatch the current parse queue across `vegeta-async-workers'."
  (let* ((queue vegeta--parse-queue)
         (lib-dir (file-name-directory (or (locate-library "vegeta-core")
                                           (error "vegeta-core.el not on load-path")))))
    (setq vegeta--parse-queue nil)
    (clrhash vegeta--parse-queued-ids)
    (dolist (batch (vegeta--split-list queue vegeta-async-workers))
      (when batch
        (vegeta--dispatch-async-batch batch lib-dir)))))

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

(defun vegeta--all-entries ()
  "Return a flat list of entries from every enabled provider.
Result is cached in `vegeta--entries-cache' until the next
`vegeta-refresh' clears it.  Background parse ticks trigger redraws
that call this repeatedly; without the cache each tick would re-scan
every project's transcripts directory and every Claude project dir."
  (or vegeta--entries-cache
      (let (all)
        (dolist (p (vegeta--enabled-providers))
          (let ((lister (plist-get p :list)))
            (when lister
              (setq all (append all (funcall lister))))))
        (setq vegeta--entries-cache all))))

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
Omits the MM-DD prefix when `date' is one of `vegeta-grouping', since
each entry then already sits under a date section header."
  (let* ((meta (vegeta--cached-meta entry))
         (ts (and meta (plist-get meta :started-at)))
         (date-grouped (memq 'date vegeta-grouping)))
    (cond
     ((and ts (string-match
               "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
               ts))
      (if date-grouped
          (format "%s:%s" (match-string 4 ts) (match-string 5 ts))
        (format "%s-%s %s:%s"
                (match-string 2 ts) (match-string 3 ts)
                (match-string 4 ts) (match-string 5 ts))))
     (t
      (let ((mtime (plist-get entry :mtime)))
        (if mtime
            (format-time-string
             (if date-grouped "%H:%M" "%m-%d %H:%M")
             (seconds-to-time mtime))
          (if date-grouped "??:??" "??-?? ??:??")))))))

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
    (funcall (or fn #'vegeta--default-date-key) entry)))

(defun vegeta--group-key (entry level)
  "Return the group key for ENTRY at LEVEL (a symbol)."
  (pcase level
    ('package (plist-get entry :provider))
    ('repo    (or (plist-get entry :repo) "(no repo)"))
    ('model   (or (vegeta--entry-agent entry) "?"))
    ('date    (vegeta--entry-date-key entry))
    (_ nil)))

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
                       (crumb (append breadcrumb (list (cons level k)))))
                  (list :level level
                        :key k
                        :breadcrumb crumb
                        :entries es
                        :children
                        (vegeta--build-tree-1 es rest crumb))))
              (nreverse order))))
        (sort nodes
              (lambda (a b)
                (let ((ma (vegeta--node-latest-mtime a))
                      (mb (vegeta--node-latest-mtime b)))
                  (cond
                   ((and (null ma) (null mb))
                    (string< (format "%s" (plist-get a :key))
                             (format "%s" (plist-get b :key))))
                   ((null ma) nil)
                   ((null mb) t)
                   (t (> ma mb))))))))))

(defun vegeta--node-latest-mtime (node)
  "Return the newest mtime among entries in NODE."
  (let ((mtimes (delq nil
                      (mapcar (lambda (e) (plist-get e :mtime))
                              (plist-get node :entries)))))
    (and mtimes (apply #'max mtimes))))

;;; Rendering

(defconst vegeta--placeholder
  (propertize "…" 'face 'vegeta-placeholder-face))

(defun vegeta--display-name (level key entries)
  "Return the display string for a group header at LEVEL with KEY."
  (pcase level
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
    (_ (format "%s" key))))

(defun vegeta--face-for-level (level)
  "Return the face used for a header at LEVEL."
  (pcase level
    ('package 'vegeta-package-face)
    ('repo    'vegeta-project-face)
    ('model   'vegeta-agent-face)
    ('date    'vegeta-date-face)
    (_        'vegeta-project-face)))

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
                (format-time-string " (updated %m-%d %H:%M)"
                                    (seconds-to-time updated))
                'face 'vegeta-updated-face)))
         (mark-str (if (eq mark 'delete)
                       (propertize "D" 'face 'vegeta-mark-face)
                     " "))
         (indent (make-string (+ 2 (* 2 depth)) ?\s))
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
          (format "%s\nProvider: %s\nAgent: %s\nSession: %s\nCwd: %s%s%s%s"
                  id
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
        (vegeta--render-tree
         (vegeta--build-tree entries vegeta-grouping)
         0)))
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

;;; Sidebar mode + keymap

(defvar vegeta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vegeta-visit)
    (define-key map (kbd "o") #'vegeta-open-transcript)
    (define-key map (kbd "g") #'vegeta-refresh)
    (define-key map (kbd "q") #'vegeta-hide-sidebar)
    (define-key map (kbd "n") #'vegeta-next-line)
    (define-key map (kbd "p") #'vegeta-previous-line)
    (define-key map (kbd "d") #'vegeta-mark-delete)
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
        (funcall visitor entry))))
   (t (user-error "Nothing at point"))))

(defun vegeta-mouse-visit (event)
  "Handle mouse click EVENT on a row."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (vegeta-visit))))

(defun vegeta-open-transcript ()
  "Open the source file for the entry at point (read-only view)."
  (interactive)
  (let ((entry (get-text-property (point) 'vegeta-entry)))
    (unless entry (user-error "No entry at point"))
    (vegeta--pop-to (find-file-noselect (plist-get entry :id)))))

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

(defun vegeta-mark-delete ()
  "Mark the entry at point for deletion."
  (interactive)
  (let ((entry (get-text-property (point) 'vegeta-entry)))
    (unless entry (user-error "No entry at point"))
    (puthash (plist-get entry :id) 'delete vegeta--marks)
    (vegeta--redraw)
    (vegeta-next-line 1)))

(defun vegeta-unmark ()
  "Remove any mark from the entry at point."
  (interactive)
  (let ((entry (get-text-property (point) 'vegeta-entry)))
    (unless entry (user-error "No entry at point"))
    (remhash (plist-get entry :id) vegeta--marks)
    (vegeta--redraw)
    (vegeta-next-line 1)))

(defun vegeta-unmark-all ()
  "Remove all deletion marks."
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

(defun vegeta-execute ()
  "Delete all entries marked for deletion (removes the source file)."
  (interactive)
  (let (ids)
    (maphash (lambda (id mark)
               (when (eq mark 'delete) (push id ids)))
             vegeta--marks)
    (cond
     ((null ids) (message "No marks to execute"))
     ((yes-or-no-p (format "Delete %d chat file(s)? " (length ids)))
      (dolist (id ids)
        (when (file-exists-p id)
          (delete-file id))
        (remhash id vegeta--parse-cache)
        (remhash id vegeta--marks))
      (vegeta--cache-mark-dirty)
      (vegeta-refresh)
      (message "Deleted %d file(s)" (length ids))))))

;;; Commands: refresh

(defun vegeta-refresh ()
  "Rescan providers and redraw.
On the first invocation of an Emacs session, the persisted cache from
`vegeta-cache-file' is loaded before the scan so entries with fresh
cached metadata render immediately.  Uncached entries are parsed in
subprocess workers when the `async' library is installed and
`vegeta-async-workers' > 0; otherwise they fall through to the
in-process idle-timer parser."
  (interactive)
  (unless vegeta--cache-loaded
    (vegeta--cache-load))
  (setq vegeta--entries-cache nil)
  (clrhash vegeta--parse-queued-ids)
  (setq vegeta--parse-queue nil)
  (let ((entries (vegeta--all-entries)))
    (vegeta--queue-uncached entries)
    (vegeta--redraw)
    (cond
     ((and vegeta--parse-queue (vegeta--async-available-p))
      (vegeta--start-async-parse))
     (vegeta--parse-queue
      (vegeta--start-parse-timer)))))

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
      (kbd "gg")  #'evil-goto-first-line
      (kbd "G")   #'evil-goto-line
      (kbd "j")   #'vegeta-next-line
      (kbd "k")   #'vegeta-previous-line
      (kbd "d")   #'vegeta-mark-delete
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
