;;; claude-code-ide.el --- Claude Code integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot, Hyunggyu Jang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.1") (websocket "1.12"))
;; Keywords: ai, claude, code, assistant, mcp, websocket
;; URL: https://github.com/manzaltu/claude-code-ide.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude Code IDE integration for Emacs provides seamless integration
;; with Claude Code through the Model Context Protocol (MCP).
;;
;; This package starts a WebSocket server that Claude Code connects to,
;; enabling real-time communication between Emacs and Claude.  It supports
;; workspace-aware sessions with automatic session isolation.
;;
;; Features:
;; - MCP WebSocket server for bidirectional communication
;; - Workspace-aware sessions with automatic workspace detection
;; - Clean session management with automatic cleanup
;; - Selection and buffer state tracking
;; - Tool support for file operations, diagnostics, and more
;;
;; Usage:
;; M-x claude-code-ide - Start Claude Code MCP server for current workspace
;; M-x claude-code-ide-stop - Stop Claude Code MCP server for current workspace
;; M-x claude-code-ide-list-sessions - List and switch between active sessions

;;; Code:

(require 'vterm)
(require 'cl-lib)
(require 'claude-code-ide-mcp)
(require 'claude-code-ide-debug)

(declare-function claude-code-ide-mcp-stop-session "claude-code-ide-mcp" (workspace-name))
(declare-function claude-code-ide-mcp-session-original-tab "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-session-client "claude-code-ide-mcp" (session))
(declare-function claude-code-ide-mcp-send-at-mentioned "claude-code-ide-mcp" ())

;; Doom Emacs function declarations
(declare-function doom-project-root "doom-lib" (&optional maybe-prompt))

;; persp-mode function declarations
(declare-function persp-name "persp-mode" (persp))
(declare-function get-current-persp "persp-mode" (&optional frame window))
(declare-function persp-add-buffer-parameter "persp-mode" (persp param-name param-value))
(declare-function persp-get-buffer-parameter "persp-mode" (persp param-name))

;;; Customization

(defgroup claude-code-ide nil
  "Claude Code integration for Emacs."
  :group 'tools
  :prefix "claude-code-ide-")

(defcustom claude-code-ide-cli-path "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-code-ide)


(defcustom claude-code-ide-cli-debug nil
  "When non-nil, launch Claude Code with the -d debug flag."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-fallback-directory "~/notes"
  "Default directory to use when no project directory can be determined."
  :type 'directory
  :group 'claude-code-ide)


;;; Constants

(defconst claude-code-ide--active-editor-notification-delay 0.1
  "Delay in seconds before sending active editor notification after connection.")

;;; Variables



(defvar claude-code-ide--workspace-sessions (make-hash-table :test 'equal)
  "Hash table mapping workspace names to Claude sessions.
Key: workspace name (string)
Value: session data structure containing directory, workspace metadata, etc.")

;;; Helper Functions


;;; Workspace Management Functions

(defun claude-code-ide--get-workspace-name ()
  "Get current workspace name for session mapping.
Requires persp-mode to be available."
  (unless (featurep 'persp-mode)
    (error "persp-mode is required for workspace management"))
  (or (persp-name (get-current-persp))
      (error "No current perspective available")))



;;; Workspace Lifecycle Hook Functions


(defun claude-code-ide--get-workspace-session (&optional workspace-name)
  "Get the session for WORKSPACE-NAME or current workspace."
  (gethash (or workspace-name (claude-code-ide--get-workspace-name))
           claude-code-ide--workspace-sessions))

(defun claude-code-ide--set-workspace-session (session &optional workspace-name)
  "Set the SESSION for WORKSPACE-NAME or current workspace."
  (puthash (or workspace-name (claude-code-ide--get-workspace-name))
           session
           claude-code-ide--workspace-sessions))

(defun claude-code-ide--clear-workspace-session (&optional workspace-name)
  "Clear the session for WORKSPACE-NAME or current workspace."
  (remhash (or workspace-name (claude-code-ide--get-workspace-name))
           claude-code-ide--workspace-sessions))

(defun claude-code-ide--cleanup-dead-workspace-sessions ()
  "Remove entries for dead sessions from the workspace sessions table.
With MCP architecture, this is now a no-op since MCP handles cleanup."
  ;; No-op: MCP sessions are managed by the MCP server
  nil)

(defun claude-code-ide--get-working-directory ()
  "Get current working directory for Claude Code IDE.
Uses fallback chain: doom-project-root → default-directory → claude-code-ide-fallback-directory."
  (cond
   ((and (featurep 'doom) (fboundp 'doom-project-root) (doom-project-root))
    (doom-project-root))
   (default-directory default-directory)
   (t (expand-file-name claude-code-ide-fallback-directory))))



(defun claude-code-ide--cleanup-all-sessions ()
  "Clean up all active Claude Code sessions."
  ;; Cleanup MCP sessions
  (maphash (lambda (workspace-name _session)
             (claude-code-ide-mcp-stop-session workspace-name))
           claude-code-ide-mcp--sessions))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-ide--cleanup-all-sessions)




;;; CLI Detection


;;; Commands


(defun claude-code-ide--start-session ()
  "Start MCP server for the current workspace.
This simplified function only handles MCP server startup for workspace-based sessions."
  (let* ((workspace-name (claude-code-ide--get-workspace-name))
         (existing-session (claude-code-ide-mcp--get-session-for-workspace workspace-name)))

    ;; If MCP session already exists for this workspace, show success message
    (if existing-session
        (claude-code-ide-log "MCP server already running for workspace '%s' on port %d"
                             workspace-name
                             (claude-code-ide-mcp-session-port existing-session))
      ;; Start new MCP server for workspace
      (let ((port (claude-code-ide-mcp-start workspace-name)))
        (claude-code-ide-log "MCP server started for workspace '%s' on port %d"
                             workspace-name
                             port)))))

;;;###autoload
(defun claude-code-ide ()
  "Start MCP server for the current workspace."
  (interactive)
  (claude-code-ide--start-session))




;;;###autoload
(defun claude-code-ide-stop ()
  "Stop the MCP server for the current workspace."
  (interactive)
  (let ((workspace-name (claude-code-ide--get-workspace-name)))
    (if (claude-code-ide-mcp--get-session-for-workspace workspace-name)
        (progn
          (claude-code-ide-mcp-stop-session workspace-name)
          (claude-code-ide-log "Stopped MCP server for workspace '%s'" workspace-name))
      (claude-code-ide-log "No MCP session running for workspace '%s'" workspace-name))))



;;;###autoload
(defun claude-code-ide-list-sessions ()
  "List all active Claude Code sessions and switch to selected one."
  (interactive)
  (let ((sessions '()))
    (maphash (lambda (workspace-name _session)
               (when (claude-code-ide-mcp--get-session-for-workspace workspace-name)
                 (push (cons workspace-name workspace-name) sessions)))
             claude-code-ide-mcp--sessions)
    (if sessions
        (let ((choice (completing-read "Switch to Claude Code session: "
                                       sessions nil t)))
          (when choice
            ;; Note: With pure MCP architecture, sessions are already active when they exist
            (claude-code-ide-log "Switched to workspace session: %s" choice)))
      (claude-code-ide-log "No active Claude Code sessions"))))

;;;###autoload
(defun claude-code-ide-insert-at-mentioned ()
  "Insert selected text into Claude prompt."
  (interactive)
  (progn
    (claude-code-ide-mcp-send-at-mentioned)
    (claude-code-ide-debug "Sent selection to Claude Code")))



;;; Workspace Hook Management


;;; Doom Workspace Command Family

;;;###autoload
(defun +workspace/claude-code (&optional arg)
  "Start or switch to Claude Code session in current workspace.
With prefix argument ARG, create new session regardless of existing session."
  (interactive "P")
  (if arg
      (claude-code-ide--create-new-workspace-session)
    (claude-code-ide--get-or-create-workspace-session)))

;;;###autoload
(defun +workspace/claude-code-kill ()
  "Kill Claude Code session in current workspace."
  (interactive)
  (claude-code-ide--kill-workspace-session))

;;;###autoload
(defun +workspace/claude-code-restart ()
  "Restart Claude Code session in current workspace."
  (interactive)
  (claude-code-ide--restart-workspace-session))

;;;###autoload
(defun +workspace/claude-code-switch ()
  "Switch between Claude Code workspace sessions."
  (interactive)
  (claude-code-ide--switch-workspace-sessions))

;;; Doom Workspace Command Implementation Functions

(defun claude-code-ide--get-or-create-workspace-session ()
  "Get existing workspace session or create new one with persistence support."
  (claude-code-ide--enhanced-start-or-switch-workspace-session))

(defun claude-code-ide--create-new-workspace-session ()
  "Create a new Claude Code session for current workspace."
  (let ((workspace-name (claude-code-ide--get-workspace-name)))
    ;; Kill existing session if it exists
    (claude-code-ide--kill-workspace-session-internal workspace-name)
    ;; Start new session
    (claude-code-ide--start-session)))

(defun claude-code-ide--kill-workspace-session (&optional workspace-name)
  "Kill Claude Code session for WORKSPACE-NAME or current workspace."
  (let ((workspace-name (or workspace-name (claude-code-ide--get-workspace-name))))
    (claude-code-ide--kill-workspace-session-internal workspace-name)))

(defun claude-code-ide--kill-workspace-session-internal (workspace-name)
  "Internal function to kill workspace session."
  (when (claude-code-ide-mcp--get-session-for-workspace workspace-name)
    (claude-code-ide-mcp-stop-session workspace-name)
    (claude-code-ide-log "MCP session killed for workspace: %s" workspace-name)))

(defun claude-code-ide--restart-workspace-session ()
  "Restart Claude Code session in current workspace."
  (let ((workspace-name (claude-code-ide--get-workspace-name)))
    (claude-code-ide--kill-workspace-session-internal workspace-name)
    ;; Small delay to ensure cleanup completes
    (run-with-timer 0.1 nil
                    (lambda ()
                      (claude-code-ide--start-session)
                      (claude-code-ide-log "Claude Code session restarted for workspace: %s" workspace-name)))))


;;; Workspace Persistence Integration

(defun claude-code-ide--serialize-session-state (session)
  "Serialize Claude session state for workspace persistence.
SESSION is a session plist containing :directory, :workspace-name, etc.
Returns a serializable plist with workspace session state."
  (when session
    (list :directory (plist-get session :directory)
          :workspace-name (plist-get session :workspace-name)
          :created (plist-get session :created)
          :last-active (plist-get session :last-active)
          :mcp-config (claude-code-ide--get-mcp-session-config
                       (plist-get session :directory)))))

(defun claude-code-ide--get-mcp-session-config (directory)
  "Get serializable MCP session configuration for DIRECTORY."
  ;; Find workspace name associated with this directory
  (let ((workspace-name nil))
    (maphash (lambda (ws-name session)
               (when (equal (plist-get session :directory) directory)
                 (setq workspace-name ws-name)))
             claude-code-ide--workspace-sessions)
    (when-let ((mcp-session (claude-code-ide-mcp--get-session-for-workspace workspace-name)))
      (list :project-dir (claude-code-ide-mcp-session-project-dir mcp-session)
            :port (claude-code-ide-mcp-session-port mcp-session)
            :original-tab (claude-code-ide-mcp-session-original-tab mcp-session)))))

(defun claude-code-ide--workspace-save-hook (persp)
  "Save Claude session state with workspace persistence.
This function is called by persp-mode when saving workspace state."
  (when (and persp (persp-name persp))
    (let ((workspace-name (persp-name persp)))
      (when-let ((session (claude-code-ide--get-workspace-session workspace-name)))
        (let ((serialized-state (claude-code-ide--serialize-session-state session)))
          (when serialized-state
            (persp-add-buffer-parameter persp 'claude-session-state serialized-state)
            (claude-code-ide-log "Saved Claude session state for workspace: %s" workspace-name)))))))

(defun claude-code-ide--workspace-restore-hook (persp)
  "Restore Claude session from workspace persistence data.
This function is called by persp-mode when loading workspace state."
  (when (and persp (persp-name persp))
    (let ((workspace-name (persp-name persp)))
      (when-let ((state (persp-get-buffer-parameter persp 'claude-session-state)))
        (claude-code-ide--restore-workspace-session workspace-name state)))))

(defun claude-code-ide--restore-workspace-session (workspace-name state)
  "Restore Claude session from serialized STATE for WORKSPACE-NAME.
Handles graceful restoration with error handling for corrupted data."
  (condition-case err
      (when (and state (listp state))
        (let ((directory (plist-get state :directory))
              (created (plist-get state :created))
              (last-active (plist-get state :last-active))
              (mcp-config (plist-get state :mcp-config)))

          ;; Validate essential state data
          (unless (and directory (file-directory-p directory))
            (claude-code-ide-log "Warning: Workspace %s session directory no longer exists: %s"
                                 workspace-name directory)
            (throw 'restore-failed "Directory does not exist"))

          ;; Don't automatically restore sessions - just log availability
          (claude-code-ide-log "Claude session state available for workspace %s (directory: %s)"
                               workspace-name
                               (file-name-nondirectory (directory-file-name directory)))

          ;; Store restoration data for on-demand restoration
          (puthash workspace-name
                   (list :state state :restored nil)
                   claude-code-ide--workspace-restore-data)))

    (error
     (claude-code-ide-log "Warning: Failed to restore Claude session for workspace %s: %s"
                          workspace-name (error-message-string err)))))

(defvar claude-code-ide--workspace-restore-data (make-hash-table :test 'equal)
  "Hash table storing workspace restoration data for on-demand restoration.")

(defun claude-code-ide--restore-session-on-demand (workspace-name)
  "Restore Claude session on-demand for WORKSPACE-NAME if restoration data exists."
  (when-let ((restore-info (gethash workspace-name claude-code-ide--workspace-restore-data)))
    (unless (plist-get restore-info :restored)
      (let ((state (plist-get restore-info :state)))
        (when state
          (let ((directory (plist-get state :directory)))
            (when (and directory (file-directory-p directory))
              ;; Set the working directory context for session creation
              (let ((default-directory directory))
                (claude-code-ide--start-session))

              ;; Mark as restored
              (puthash workspace-name
                       (plist-put restore-info :restored t)
                       claude-code-ide--workspace-restore-data)

              (claude-code-ide-log "Restored Claude session for workspace: %s" workspace-name)
              t)))))))

(defun claude-code-ide--enhanced-start-or-switch-workspace-session ()
  "Enhanced version that attempts restoration before creating new session."
  (let ((workspace-name (claude-code-ide--get-workspace-name)))
    (if (claude-code-ide-mcp--get-session-for-workspace workspace-name)
        ;; MCP session already exists and is active
        (claude-code-ide-log "Using existing MCP session for workspace: %s" workspace-name)
      ;; No existing session - try restoration first
      (unless (claude-code-ide--restore-session-on-demand workspace-name)
        ;; No restoration data - create new session
        (claude-code-ide--start-session)))))

;; Hook registration with persp-mode
(defun claude-code-ide--register-persistence-hooks ()
  "Register workspace persistence hooks with persp-mode."
  (when (featurep 'persp-mode)
    (add-hook 'persp-before-save-state-to-file-functions
              #'claude-code-ide--workspace-save-hook)
    (add-hook 'persp-after-load-state-from-file-functions
              #'claude-code-ide--workspace-restore-hook)
    (claude-code-ide-log "Registered Claude workspace persistence hooks")))

(defun claude-code-ide--unregister-persistence-hooks ()
  "Unregister workspace persistence hooks from persp-mode."
  (when (featurep 'persp-mode)
    (remove-hook 'persp-before-save-state-to-file-functions
                 #'claude-code-ide--workspace-save-hook)
    (remove-hook 'persp-after-load-state-from-file-functions
                 #'claude-code-ide--workspace-restore-hook)
    (claude-code-ide-log "Unregistered Claude workspace persistence hooks")))

;; Auto-register hooks when module loads
;;; VTerm Integration for Unmanaged Terminals

(defun claude-code-ide--vterm-mode-advice (orig-fun &rest args)
  "Advice for vterm-mode to set up workspace environment before process creation."
  (let ((vterm-environment vterm-environment)) ;; vterm-environment should be loaded when `vterm-mode' is called
    (when-let* ((workspace (claude-code-ide--get-workspace-name))
                (port (claude-code-ide-mcp-get-workspace-port workspace)))
      (claude-code-ide-debug "Setting up vterm environment for workspace %s (port %d)" workspace port)
      (cl-callf2 append
          (list (format "CLAUDE_CODE_SSE_PORT=%d" port)
                "ENABLE_IDE_INTEGRATION=true"
                "TERM_PROGRAM=emacs"
                "FORCE_CODE_TERMINAL=true")
          vterm-environment)
      (claude-code-ide-debug "Vterm environment configured: %s" vterm-environment))
    (apply orig-fun args)))

(defcustom claude-code-ide-vterm-anti-flicker t
  "Enable intelligent flicker reduction for vterm display.
When enabled, this feature optimizes terminal rendering by detecting
and batching rapid update sequences.  This provides smoother visual
output during complex terminal operations such as expanding text areas
and rapid screen updates.

This optimization applies only to vterm and uses advanced pattern
matching to maintain responsiveness while improving visual quality."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-vterm-render-delay 0.005
  "Rendering optimization delay for batched terminal updates.
This parameter defines the collection window for related terminal
update sequences when anti-flicker mode is active.  The timing
balances visual smoothness with interaction responsiveness.

The 0.005 second (5ms) default delivers optimal rendering quality
with imperceptible latency."
  :type 'number
  :group 'claude-code-ide)

(defvar-local claude-code-ide--vterm-render-queue nil
  "Queue for optimizing terminal rendering sequences.")

(defvar-local claude-code-ide--vterm-render-timer nil
  "Timer for executing queued rendering operations.")

(defvar-local claude-code-ide--copy-mode t)

(defun claude-code-ide--vterm-smart-renderer (orig-fun process input)
  "Smart rendering filter for optimized vterm display updates.
This advanced filter analyzes terminal output patterns to identify
rapid update sequences that benefit from batched processing.
It significantly improves visual quality during complex operations.

ORIG-FUN is the underlying filter to enhance.
PROCESS is the terminal process being optimized.
INPUT contains the terminal output stream."
  (with-current-buffer (process-buffer process)
    ;; Detect rapid terminal redraw sequences
    ;; Pattern analysis for complex terminal updates:
    ;; - Vertical cursor movements (ESC[<n>A)
    ;; - Line clearing operations (ESC[K)
    ;; - High escape sequence density
    (let* ((complex-redraw-detected
            ;; Pattern: vertical movement + clear, repeated
            (string-match-p "\033\\[[0-9]*A.*\033\\[K.*\033\\[[0-9]*A.*\033\\[K" input))
           (clear-count (cl-count-if (lambda (s) (string= s "\033[K"))
                                     (split-string input "\033\\[K" t)))
           (escape-count (cl-count ?\033 input))
           (input-length (length input))
           ;; High escape density indicates redrawing, not normal output
           (escape-density (if (> input-length 0)
                               (/ (float escape-count) input-length)
                             0)))
      ;; Optimize rendering for detected patterns:
      ;; 1. Complex redraw sequence detected, OR
      ;; 2. Escape sequence density exceeds threshold with line operations
      ;; 3. OR already queuing (to complete the sequence)
      (if (or complex-redraw-detected
              (and (> escape-density 0.3)
                   (>= clear-count 2))
              claude-code-ide--vterm-render-queue)
          (progn
            ;; Add to buffer
            (setq claude-code-ide--vterm-render-queue
                  (concat claude-code-ide--vterm-render-queue input))
            ;; Reset existing render timer
            (when claude-code-ide--vterm-render-timer
              (cancel-timer claude-code-ide--vterm-render-timer))
            ;; Schedule optimized rendering
            ;; Timing calibrated for visual quality
            (setq claude-code-ide--vterm-render-timer
                  (run-at-time claude-code-ide-vterm-render-delay nil
                               (lambda (buf)
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (when claude-code-ide--vterm-render-queue
                                       (let ((inhibit-redisplay t)
                                             (data claude-code-ide--vterm-render-queue))
                                         ;; Clear queue first to prevent recursion
                                         (setq claude-code-ide--vterm-render-queue nil
                                               claude-code-ide--vterm-render-timer nil)
                                         ;; Execute queued rendering
                                         (funcall orig-fun
                                                  (get-buffer-process buf)
                                                  data))))))
                               (current-buffer))))
        ;; Standard processing for regular output
        (funcall orig-fun process input)))))

(defun claude-code-ide--vterm-toggle-scroll (&rest _)
  (when (and claude-code-ide--copy-mode (claude-code-ide--session-buffer-p (current-buffer)))
    (if (> (window-end) (buffer-size))
        (when vterm-copy-mode (vterm-copy-mode-done nil))
      (vterm-copy-mode 1))))

(defcustom claude-code-ide-prevent-reflow-glitch t
  "Workaround for Claude Code terminal scrolling bug #1422.
When non-nil (default), prevents the terminal from reflowing on height-only
changes which can trigger uncontrollable scrolling in Claude Code.
See: https://github.com/anthropics/claude-code/issues/1422
This setting should be removed once the upstream bug is fixed."
  :type 'boolean
  :group 'claude-code-ide)

;;; Terminal Reflow Glitch Prevention
;;
;; This section implements a workaround for Claude Code bug #1422
;; where terminal reflows during height-only changes can cause
;; uncontrollable scrolling. This code should be removed once
;; the upstream bug is fixed.
;; See: https://github.com/anthropics/claude-code/issues/1422

(defun claude-code-ide--terminal-resize-handler ()
  "Retrieve the terminal's resize handling function based on backend."
  #'vterm--window-adjust-process-window-size)

(defun claude-code-ide--terminal-scroll-mode-active-p ()
  "Determine if terminal is currently in scroll/copy mode."
  (bound-and-true-p vterm-copy-mode))

(defun claude-code-ide--session-buffer-p (buffer)
  "Check if BUFFER belongs to a Claude Code session."
  (buffer-local-value '+vterm--id buffer))

(defun claude-code-ide--terminal-reflow-filter (original-fn &rest args)
  "Filter terminal reflows to prevent height-only resize triggers.
This wraps ORIGINAL-FN to suppress reflow signals unless the terminal
width has actually changed, working around the scrolling glitch."
  (let* ((base-result (apply original-fn args))
         (dimensions-stable t))
    ;; Examine each window showing a Claude session
    (dolist (win (window-list))
      (when-let* ((buf (window-buffer win))
                  ((claude-code-ide--session-buffer-p buf)))
        (let* ((new-width (window-width win))
               (cached-width (window-parameter win 'claude-code-ide-cached-width)))
          ;; Width change detected
          (unless (eql new-width cached-width)
            (setq dimensions-stable nil)
            (set-window-parameter win 'claude-code-ide-cached-width new-width)))))
    ;; Decide whether to allow reflow
    (cond
     ;; Not in a Claude buffer - pass through
     ((not (claude-code-ide--session-buffer-p (current-buffer)))
      base-result)
     ;; In scroll mode - suppress reflow
     ((claude-code-ide--terminal-scroll-mode-active-p)
      nil)
     ;; Dimensions changed - allow reflow
     ((not dimensions-stable)
      base-result)
     ;; No width change - suppress reflow
     (t nil))))

(defun claude-code-ide--enable-vterm-integration ()
  "Enable automatic vterm integration for unmanaged terminals."
  (when (fboundp 'vterm-mode)
    (advice-add 'vterm-mode :around #'claude-code-ide--vterm-mode-advice)
    ;; Configure vterm for enhanced performance and visual quality.
    ;; Establishes optimal terminal settings including rendering optimizations,
    ;; cursor management, and process buffering for superior user experience."
    (when claude-code-ide-vterm-anti-flicker
      (advice-add 'vterm--filter :around #'claude-code-ide--vterm-smart-renderer)
      (advice-add 'set-window-vscroll :after #'claude-code-ide--vterm-toggle-scroll))
    (when claude-code-ide-prevent-reflow-glitch
      (advice-add (claude-code-ide--terminal-resize-handler)
                  :around #'claude-code-ide--terminal-reflow-filter))))

(defun claude-code-ide--disable-vterm-integration ()
  "Disable automatic vterm integration."
  (when (fboundp 'vterm-mode)
    (advice-remove 'vterm-mode #'claude-code-ide--vterm-mode-advice)
    (when claude-code-ide-vterm-anti-flicker
      (advice-remove 'vterm--filter #'claude-code-ide--vterm-smart-renderer)
      (advice-add 'set-window-vscroll #'claude-code-ide--vterm-toggle-scroll))
    (when claude-code-ide-prevent-reflow-glitch
      (advice-remove (claude-code-ide--terminal-resize-handler)
                     #'claude-code-ide--terminal-reflow-filter))))

;;;###autoload
(defun claude-code-toggle-auto-copy-mode ()
  (interactive)
  (setq (not claude-code-ide--copy-mode)))

;; Enable vterm integration by default
(claude-code-ide--enable-vterm-integration)

(when (featurep 'persp-mode)
  (claude-code-ide--register-persistence-hooks))

(provide 'claude-code-ide)

;;; claude-code-ide.el ends here
