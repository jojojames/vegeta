;;; vegeta.el --- Sidebar browser for AI agent chats -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.60") (project "0.9"))

;;; Commentary:
;;
;; Persistent sidebar that lists chats/conversations from AI agent tools
;; installed on this machine.  Each source is a "provider" (agent-shell,
;; Claude CLI, etc.); entries can be nested and grouped by any combination
;; of `package', `repo', `model', and `date' via `vegeta-grouping'.
;;
;; RET on a chat row dispatches through the entry's provider:
;;   - agent-shell entries: resume via `agent-shell--start' when a session
;;     id is present, else start a fresh shell in the recorded cwd.
;;   - Claude CLI entries: spawn `claude --resume <uuid>' in a terminal
;;     (ghostel/vterm/term) at the recorded cwd.
;;
;; Metadata parsing (agent name, first user prompt, model) runs lazily on
;; an idle-timer in chunks so hundreds of transcripts don't block on
;; refresh.  Parse results are cached keyed by (path . mtime) plus a
;; schema version so parser changes invalidate stale entries.
;;
;; This file is a thin loader.  Everything lives in:
;;   - `vegeta-core'        — sidebar, mode, grouping, rendering, registry
;;   - `vegeta-agent-shell' — agent-shell provider (markdown transcripts)
;;   - `vegeta-claude-cli'  — Claude CLI provider (JSONL sessions)
;;
;; Providers self-register on load via `vegeta-register-provider'.

;;; Code:

(require 'vegeta-core)
(require 'vegeta-agent-shell)
(require 'vegeta-claude-cli)
(require 'vegeta-antigravity-cli)
(require 'vegeta-antigravity)

(provide 'vegeta)
;;; vegeta.el ends here
