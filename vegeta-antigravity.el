;;; vegeta-antigravity.el --- Google Antigravity IDE provider -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, antigravity, gemini, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Lists conversations produced by the Antigravity IDE (as opposed to
;; the `agy' CLI, which is covered by `vegeta-antigravity-cli.el').
;;
;; Storage layout at ~/.gemini/antigravity/:
;;
;;   agyhub_summaries_proto.pb          - single protobuf file listing
;;                                        every IDE conversation with
;;                                        title, workspace URI, and
;;                                        start/update timestamps.
;;   brain/<UUID>/                      - per-conversation artifacts
;;                                        (task.md, walkthrough.md, ...)
;;   conversations/<UUID>.pb            - encrypted per-conversation
;;                                        state (opaque to us)
;;
;; The IDE encrypts individual conversation files, but the summaries
;; proto is a plain protobuf with everything we need for a browsable
;; listing.  We read it with a small hand-rolled protobuf decoder so
;; there's no external dependency.
;;
;; RET on a row asks macOS to open the IDE at the conversation's
;; workspace folder (`open -a "Antigravity IDE" <cwd>').  Antigravity's
;; CLI has no deep-link for jumping to a specific chat, so from there
;; the user picks the chat from Antigravity's own chat panel.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(require 'vegeta-core)

;;; Customization

(defcustom vegeta-antigravity-data-dir
  (expand-file-name "~/.gemini/antigravity/")
  "Root data directory for the Antigravity IDE."
  :type 'directory
  :group 'vegeta)

(defcustom vegeta-antigravity-summaries-file
  "agyhub_summaries_proto.pb"
  "Filename of the protobuf holding every IDE conversation's summary.
Resolved relative to `vegeta-antigravity-data-dir'."
  :type 'string
  :group 'vegeta)

(defcustom vegeta-antigravity-open-command
  (cond ((eq system-type 'darwin) '("open" "-a" "Antigravity IDE"))
        (t '("antigravity-ide")))
  "Command + args used by `:visit' to launch the IDE at a workspace.
The workspace path is appended as the final argument."
  :type '(repeat string)
  :group 'vegeta)

;;; Minimal protobuf decoder (wire format only)

(defun vegeta--pb-varint (str pos)
  "Read a varint at POS in unibyte STR.  Return (VALUE . NEW-POS)."
  (let ((result 0) (shift 0) done)
    (while (not done)
      (let ((b (aref str pos)))
        (setq pos (1+ pos)
              result (logior result (ash (logand b #x7f) shift)))
        (if (zerop (logand b #x80))
            (setq done t)
          (setq shift (+ shift 7)))))
    (cons result pos)))

(defun vegeta--pb-parse (str &optional start end)
  "Parse protobuf STR between START (default 0) and END (default full).
Return list of (FIELD-NUMBER . VALUE) in the order encountered.  VALUE is
an integer for varint fields, a unibyte string for length-delimited
fields, and skipped over for fixed32/fixed64 fields.  Malformed input
terminates parsing silently."
  (let ((pos (or start 0))
        (end (or end (length str)))
        (result nil))
    (condition-case _
        (while (< pos end)
          (let* ((tag-pair (vegeta--pb-varint str pos))
                 (tag (car tag-pair))
                 (fno (ash tag -3))
                 (wtype (logand tag 7)))
            (setq pos (cdr tag-pair))
            (pcase wtype
              (0
               (let ((v (vegeta--pb-varint str pos)))
                 (push (cons fno (car v)) result)
                 (setq pos (cdr v))))
              (2
               (let* ((len-pair (vegeta--pb-varint str pos))
                      (len (car len-pair))
                      (data-start (cdr len-pair)))
                 (push (cons fno (substring str data-start
                                            (+ data-start len)))
                       result)
                 (setq pos (+ data-start len))))
              (5 (setq pos (+ pos 4)))
              (1 (setq pos (+ pos 8)))
              (_ (setq pos end)))))
      (error nil))
    (nreverse result)))

(defun vegeta--pb-field (parsed fno)
  "Return the first value in PARSED with FIELD-NUMBER FNO, or nil."
  (cdr (assq fno parsed)))

(defun vegeta--pb-string (parsed fno)
  "Return a UTF-8 decoded string for length-delimited field FNO in PARSED."
  (when-let* ((raw (vegeta--pb-field parsed fno)))
    (decode-coding-string raw 'utf-8)))

(defun vegeta--pb-timestamp (bytes)
  "Decode a protobuf Timestamp BYTES into an ISO-8601 UTC string.
Timestamp has field 1 seconds, field 2 nanos (both varints)."
  (when (and bytes (stringp bytes) (> (length bytes) 0))
    (let* ((fields (vegeta--pb-parse bytes))
           (secs (or (vegeta--pb-field fields 1) 0)))
      (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time secs) t))))

(defun vegeta--pb-workspace-cwd (bytes)
  "Extract the workspace path from the summary's field 9 BYTES.
Field 9 contains repeated string subfields; the first is a `file://'
URI for the project root.  Returns the decoded path even when the
directory no longer exists on disk — old conversations for deleted
projects still deserve to be listed under their original workspace."
  (when-let* ((_ bytes)
              (fields (vegeta--pb-parse bytes))
              (uri (vegeta--pb-string fields 1))
              (_ (string-prefix-p "file://" uri)))
    (let ((path (url-unhex-string (substring uri 7))))
      (and path (file-name-as-directory (expand-file-name path))))))

;;; Summaries reading

(defun vegeta--antigravity-summaries-path ()
  "Return the absolute path to the IDE's summaries protobuf file."
  (expand-file-name vegeta-antigravity-summaries-file
                    vegeta-antigravity-data-dir))

(defun vegeta--antigravity-summaries ()
  "Return alist mapping UUID -> summary plist for every IDE conversation.
Summary plist keys: `:title', `:cwd', `:started-at', `:updated-at',
`:msg-count'."
  (let ((path (vegeta--antigravity-summaries-path)))
    (when (file-readable-p path)
      (condition-case err
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (let* ((raw (buffer-substring-no-properties
                         (point-min) (point-max)))
                   (top (vegeta--pb-parse raw))
                   result)
              (dolist (entry top)
                (when (eq (car entry) 1)
                  (let* ((inner (vegeta--pb-parse (cdr entry)))
                         (uuid (vegeta--pb-string inner 1))
                         (payload (vegeta--pb-field inner 2)))
                    (when (and uuid payload)
                      (let* ((sub (vegeta--pb-parse payload))
                             (title (vegeta--pb-string sub 1))
                             (msg (vegeta--pb-field sub 2))
                             (started (vegeta--pb-timestamp
                                       (vegeta--pb-field sub 3)))
                             (updated (vegeta--pb-timestamp
                                       (vegeta--pb-field sub 7)))
                             (cwd (vegeta--pb-workspace-cwd
                                   (vegeta--pb-field sub 9))))
                        (push (cons uuid
                                    (list :title title
                                          :cwd cwd
                                          :started-at started
                                          :updated-at updated
                                          :msg-count msg))
                              result))))))
              (nreverse result)))
        (error
         (message "vegeta[antigravity]: failed to parse %s: %S"
                  path err)
         nil)))))

;;; Provider: :list

(defun vegeta--antigravity-brain-dir (uuid)
  "Return the brain directory path for IDE conversation UUID."
  (expand-file-name (concat "brain/" uuid "/")
                    vegeta-antigravity-data-dir))

(defun vegeta--antigravity-conversation-pb (uuid)
  "Return the encrypted conversation.pb path for UUID."
  (expand-file-name (concat "conversations/" uuid ".pb")
                    vegeta-antigravity-data-dir))

(defun vegeta--antigravity-list ()
  "List all IDE conversations by cross-referencing the summaries proto
with the on-disk brain directories."
  (let (entries)
    (dolist (pair (vegeta--antigravity-summaries))
      (let* ((uuid (car pair))
             (summary (cdr pair))
             (brain (vegeta--antigravity-brain-dir uuid)))
        (when (file-directory-p brain)
          (let ((mtime (float-time
                        (file-attribute-modification-time
                         (file-attributes brain)))))
            (push (list :provider 'antigravity
                        :id brain
                        :repo (plist-get summary :cwd)
                        :agent "Gemini"
                        :mtime mtime
                        :extras (list :uuid uuid
                                      :summary summary
                                      :brain brain
                                      :pb (vegeta--antigravity-conversation-pb uuid)))
                  entries)))))
    entries))

;;; Provider: :parse

(defun vegeta--antigravity-parse (entry)
  "Return meta plist for an IDE ENTRY straight from its cached summary.
The IDE encrypts per-conversation transcripts, so there is nothing
more detailed to extract from disk — the summary is the whole story."
  (let* ((extras (plist-get entry :extras))
         (uuid (plist-get extras :uuid))
         (s (plist-get extras :summary)))
    (list :agent "Gemini"
          :model nil
          :started-at (plist-get s :started-at)
          :updated-at (plist-get s :updated-at)
          :cwd (plist-get s :cwd)
          :session-id uuid
          :first-prompt nil
          :ai-title (plist-get s :title)
          :renamed nil)))

;;; Provider: :date-key

(defun vegeta--antigravity-date-key (entry)
  "Return YYYY-MM-DD for IDE ENTRY.
Uses the summary's `:started-at' (populated at list time and cached),
falling back to file mtime."
  (let* ((s (plist-get (plist-get entry :extras) :summary))
         (started (and s (plist-get s :started-at))))
    (cond
     ((and started (string-match
                    "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
                    started))
      (match-string 1 started))
     (t (vegeta--default-date-key entry)))))

;;; Provider: :visit

(defun vegeta--antigravity-visit (entry)
  "Open the Antigravity IDE at ENTRY's workspace folder.
The IDE has no CLI/URL-scheme deep-link to a specific conversation,
so this opens the workspace and leaves it to the user to pick the
chat from Antigravity's own chat panel.  If the recorded workspace
no longer exists on disk (project was deleted), launches the IDE
without a folder argument so it at least opens something."
  (let* ((cwd-raw (plist-get entry :repo))
         (cwd (and (stringp cwd-raw)
                   (file-directory-p cwd-raw)
                   (expand-file-name cwd-raw)))
         (cmd (car vegeta-antigravity-open-command))
         (args (append (cdr vegeta-antigravity-open-command)
                       (and cwd (list cwd)))))
    (message "vegeta[antigravity]: %s %s%s"
             cmd (string-join args " ")
             (if cwd "" "  (workspace missing; opening IDE bare)"))
    (apply #'start-process
           (format "vegeta-antigravity-open-%s"
                   (substring (or (plist-get (plist-get entry :extras) :uuid)
                                  "?")
                              0 8))
           nil cmd args)))

;;; Provider: :delete

(defun vegeta--antigravity-delete (entry)
  "Remove ENTRY's on-disk IDE state.
Deletes the brain directory and the encrypted `.pb' file.  Note the
conversation may still exist server-side and could re-sync on next
IDE launch — Antigravity has no CLI-level `delete session' operation,
so this is best-effort local cleanup."
  (let* ((extras (plist-get entry :extras))
         (brain (plist-get extras :brain))
         (pb (plist-get extras :pb)))
    (when (and brain (file-directory-p brain))
      (delete-directory brain t))
    (when (and pb (file-exists-p pb))
      (delete-file pb))))

;;; Registration

(vegeta-register-provider
 (list :id 'antigravity
       :name "Antigravity"
       :list #'vegeta--antigravity-list
       :parse #'vegeta--antigravity-parse
       :visit #'vegeta--antigravity-visit
       :date-key #'vegeta--antigravity-date-key
       :delete #'vegeta--antigravity-delete
       ;; `:visit' launches the local Antigravity IDE (there is no remote
       ;; deep-link), so the provider is local-only.
       :local-only t
       ;; Every IDE conversation is Gemini; skip the redundant model level.
       :skip-levels '(model)))

(provide 'vegeta-antigravity)
;;; vegeta-antigravity.el ends here
