;;; claude-code-ide.el --- Claude Code integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
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
;; with Claude Code CLI through the Model Context Protocol (MCP).
;;
;; This package starts a WebSocket server that Claude Code CLI connects to,
;; enabling real-time communication between Emacs and Claude.  It supports
;; multiple concurrent sessions per project.
;;
;; Features:
;; - Automatic IDE mode activation when starting Claude
;; - MCP WebSocket server for bidirectional communication
;; - Project-aware sessions with automatic working directory detection
;; - Clean session management with automatic cleanup on exit
;; - Selection and buffer state tracking
;; - Tool support for file operations, diagnostics, and more
;;
;; Usage:
;; M-x claude-code-ide - Start Claude Code for current project
;; M-x claude-code-ide-resume - Resume Claude Code with previous conversation
;; M-x claude-code-ide-stop - Stop Claude Code for current project
;; M-x claude-code-ide-switch-to-buffer - Switch to project's Claude buffer
;; M-x claude-code-ide-list-sessions - List and switch between all sessions
;; M-x claude-code-ide-check-status - Check CLI availability and version
;; M-x claude-code-ide-insert-at-mentioned - Send selected text to Claude

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
(declare-function map! "doom-keybinds" (&rest rest))
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

(defcustom claude-code-ide-buffer-name-function #'claude-code-ide--default-buffer-name
  "Function to generate buffer names for Claude Code sessions.
The function is called with one argument, the working directory,
and should return a string to use as the buffer name."
  :type 'function
  :group 'claude-code-ide)

(defcustom claude-code-ide-cli-debug nil
  "When non-nil, launch Claude Code with the -d debug flag."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-side 'right
  "Side of the frame where the Claude Code window should appear.
Can be `'left', `'right', `'top', or `'bottom'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-width 90
  "Width of the Claude Code side window when opened on left or right."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-height 20
  "Height of the Claude Code side window when opened on top or bottom."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-on-open t
  "Whether to focus the Claude Code window when it opens."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-claude-after-ediff t
  "Whether to focus the Claude Code window after opening ediff.
When non-nil (default), focus returns to the Claude Code window
after opening ediff.  When nil, focus remains on the ediff control
window, allowing direct interaction with the diff controls."
  :type 'boolean
  :group 'claude-code-ide)

;;; Constants

(defconst claude-code-ide--active-editor-notification-delay 0.1
  "Delay in seconds before sending active editor notification after connection.")

;;; Variables

(defvar claude-code-ide--cli-available nil
  "Whether Claude Code CLI is available and detected.")


(defvar claude-code-ide--workspace-sessions (make-hash-table :test 'equal)
  "Hash table mapping workspace names to Claude sessions.
Key: workspace name (string)
Value: session data structure containing process, directory, and other metadata.")

;;; Helper Functions

(defun claude-code-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*claude-code[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

;;; Workspace Management Functions

(defun claude-code-ide--get-workspace-name ()
  "Get current workspace name for session mapping.
Uses fallback chain: persp-mode → directory-based naming → default."
  (cond
   ((and (featurep 'persp-mode) (fboundp 'persp-name) (fboundp 'get-current-persp) (persp-name (get-current-persp)))
    (persp-name (get-current-persp)))
   ((claude-code-ide--get-working-directory)
    (file-name-nondirectory (directory-file-name (claude-code-ide--get-working-directory))))
   (t "default")))

(defun claude-code-ide--switch-to-workspace-session (workspace-name)
  "Switch to the Claude session associated with WORKSPACE-NAME.
If a session exists for the workspace, display it.
If no session exists, do nothing (user can start one manually)."
  (when-let ((session (claude-code-ide--get-workspace-session workspace-name)))
    (let* ((buffer (plist-get session :buffer))
           (process (plist-get session :process)))
      (when (and buffer (buffer-live-p buffer) process (process-live-p process))
        (claude-code-ide--display-buffer-in-side-window buffer)
        (claude-code-ide-debug "Switched to Claude session for workspace: %s" workspace-name)))))

(defun claude-code-ide--save-current-session-state ()
  "Save current session state before workspace switch.
This ensures session data is preserved during transitions."
  (let* ((workspace-name (claude-code-ide--get-workspace-name))
         (session (claude-code-ide--get-workspace-session workspace-name)))
    (when session
      (let ((buffer (plist-get session :buffer)))
        (when (and buffer (buffer-live-p buffer))
          ;; Update session metadata with current state
          (claude-code-ide--set-workspace-session
           (plist-put session :last-active (current-time))
           workspace-name)
          (claude-code-ide-debug "Saved session state for workspace: %s" workspace-name))))))

;;; Workspace Lifecycle Hook Functions

(defun claude-code-ide--workspace-activated-h (type)
  "Handle workspace activation for Claude Code IDE.
Switches to the Claude session associated with the activated workspace.
This hook is called when persp-mode activates a workspace.
TYPE is either 'frame or 'window indicating the scope of activation."
  (when-let ((persp (get-current-persp)))
    (let ((workspace-name (persp-name persp)))
      (claude-code-ide-debug "Workspace activated: %s (type: %s)" workspace-name type)
      (claude-code-ide--switch-to-workspace-session workspace-name))))

(defun claude-code-ide--workspace-deactivated-h (type)
  "Handle workspace deactivation for Claude Code IDE.
Saves current session state before workspace switch.
This hook is called before persp-mode deactivates a workspace.
TYPE is either 'frame or 'window indicating the scope of deactivation."
  (when-let ((persp (get-current-persp)))
    (let ((workspace-name (persp-name persp)))
      (claude-code-ide-debug "Workspace deactivating: %s (type: %s)" workspace-name type)
      (claude-code-ide--save-current-session-state))))

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
  "Remove entries for dead sessions from the workspace sessions table."
  (maphash (lambda (workspace-name session)
             (let ((process (plist-get session :process)))
               (unless (and process (process-live-p process))
                 (remhash workspace-name claude-code-ide--workspace-sessions))))
           claude-code-ide--workspace-sessions))

(defun claude-code-ide--get-working-directory ()
  "Get current working directory for Claude Code IDE.
Uses fallback chain: doom-project-root → default-directory → original constant."
  (cond
   ((and (featurep 'doom) (fboundp 'doom-project-root) (doom-project-root))
    (doom-project-root))
   (default-directory default-directory)
   (t (expand-file-name "~/notes"))))

(defun claude-code-ide--get-buffer-name (&optional directory)
  "Get the buffer name for the Claude Code session in DIRECTORY.
If DIRECTORY is not provided, use the current working directory."
  (funcall claude-code-ide-buffer-name-function
           (or directory (claude-code-ide--get-working-directory))))


(defun claude-code-ide--cleanup-all-sessions ()
  "Clean up all active Claude Code sessions."
  ;; Cleanup workspace sessions directly
  (maphash (lambda (workspace-name session)
             (let ((process (plist-get session :process))
                   (directory (plist-get session :directory)))
               (when (and process (process-live-p process) directory)
                 (claude-code-ide--cleanup-on-exit directory))))
           claude-code-ide--workspace-sessions))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-code-ide--cleanup-all-sessions)

(defun claude-code-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`claude-code-ide-window-side' with dimensions from
`claude-code-ide-window-width' or `claude-code-ide-window-height'.
If `claude-code-ide-focus-on-open' is non-nil, the window is selected."
  (let* ((side claude-code-ide-window-side)
         (slot 0)
         (window-parameters '((no-delete-other-windows . t)))
         (display-buffer-alist
          `((,(regexp-quote (buffer-name buffer))
             (display-buffer-in-side-window)
             (side . ,side)
             (slot . ,slot)
             (window-width . ,(if (memq side '(left right))
                                  claude-code-ide-window-width
                                'fit-window-to-buffer))
             (window-height . ,(if (memq side '(top bottom))
                                   claude-code-ide-window-height
                                 'fit-window-to-buffer))
             (window-parameters . ,window-parameters)))))
    (let ((window (display-buffer buffer)))
      ;; Select the window to give it focus if configured to do so
      (when (and window claude-code-ide-focus-on-open)
        (select-window window))
      window)))

(defvar claude-code-ide--cleanup-in-progress nil
  "Flag to prevent recursive cleanup calls.")

(defun claude-code-ide--cleanup-on-exit (directory)
  "Clean up MCP server and process tracking when Claude exits for DIRECTORY."
  (unless claude-code-ide--cleanup-in-progress
    (setq claude-code-ide--cleanup-in-progress t)
    (unwind-protect
        (progn
          ;; Find and remove corresponding workspace session
          (maphash (lambda (workspace-name session)
                     (when (equal (plist-get session :directory) directory)
                       (claude-code-ide--clear-workspace-session workspace-name)))
                   claude-code-ide--workspace-sessions)
          ;; Stop MCP server for this workspace
          (let ((workspace-name (claude-code-ide--get-workspace-name)))
            (claude-code-ide-mcp-stop-session workspace-name))
          ;; Kill the vterm buffer if it exists
          (let ((buffer-name (claude-code-ide--get-buffer-name directory)))
            (when-let ((buffer (get-buffer buffer-name)))
              (when (buffer-live-p buffer)
                (let ((kill-buffer-hook nil) ; Disable hooks to prevent recursion
                      (kill-buffer-query-functions nil)) ; Don't ask for confirmation
                  (kill-buffer buffer)))))
          (claude-code-ide-debug "Cleaned up Claude Code session for %s"
                                 (file-name-nondirectory (directory-file-name directory))))
      (setq claude-code-ide--cleanup-in-progress nil))))

;;; CLI Detection

(defun claude-code-ide--detect-cli ()
  "Detect if Claude Code CLI is available."
  (let ((available (condition-case nil
                       (eq (call-process claude-code-ide-cli-path nil nil nil "--version") 0)
                     (error nil))))
    (setq claude-code-ide--cli-available available)))

(defun claude-code-ide--ensure-cli ()
  "Ensure Claude Code CLI is available, detect if needed."
  (unless claude-code-ide--cli-available
    (claude-code-ide--detect-cli))
  claude-code-ide--cli-available)

;;; Commands

(defun claude-code-ide--toggle-existing-window (existing-buffer working-dir)
  "Toggle visibility of EXISTING-BUFFER window for WORKING-DIR.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window."
  (let ((window (get-buffer-window existing-buffer)))
    (if window
        ;; Window is visible, hide it
        (progn
          (delete-window window)
          (claude-code-ide-debug "Claude Code window hidden"))
      ;; Window is not visible, show it
      (progn
        (claude-code-ide--display-buffer-in-side-window existing-buffer)
        ;; Update the original tab when showing the window
        (when-let ((session (claude-code-ide-mcp--get-session-for-workspace (claude-code-ide--get-workspace-name))))
          (when (fboundp 'tab-bar--current-tab)
            (setf (claude-code-ide-mcp-session-original-tab session) (tab-bar--current-tab))))
        (claude-code-ide-debug "Claude Code window shown")))))

(defun claude-code-ide--build-claude-command (&optional resume)
  "Build the Claude command with optional flags.
If RESUME is non-nil, add the -r flag.
If `claude-code-ide-cli-debug' is non-nil, add the -d flag."
  (let ((claude-cmd claude-code-ide-cli-path))
    ;; Add debug flag if enabled
    (when claude-code-ide-cli-debug
      (setq claude-cmd (concat claude-cmd " -d")))
    ;; Add resume flag if requested
    (when resume
      (setq claude-cmd (concat claude-cmd " -r")))
    claude-cmd))

(defun claude-code-ide--create-vterm-session (buffer-name working-dir port resume &optional terminal-only)
  "Create a new vterm session for Claude Code.
BUFFER-NAME is the name for the vterm buffer.
WORKING-DIR is the working directory.
PORT is the MCP server port.
RESUME is whether to resume a previous conversation.
TERMINAL-ONLY is whether to start just the terminal without auto-launching Claude.

Returns a cons cell of (buffer . process) on success.
Signals an error if vterm fails to initialize."
  (let* ((claude-cmd (claude-code-ide--build-claude-command resume))
         (vterm-buffer-name buffer-name)
         (default-directory working-dir)
         ;; Set vterm-shell based on terminal-only mode
         (vterm-shell (if terminal-only
                          (or (getenv "SHELL") "/bin/bash")  ; Use default shell for terminal-only
                        claude-cmd))  ; Auto-launch Claude for normal mode
         ;; vterm uses vterm-environment for passing env vars
         (vterm-environment (append
                             (list (format "CLAUDE_CODE_SSE_PORT=%d" port)
                                   "ENABLE_IDE_INTEGRATION=true"
                                   "TERM_PROGRAM=emacs"
                                   "FORCE_CODE_TERMINAL=true")
                             vterm-environment)))
    ;; Create vterm buffer without switching to it
    (let ((buffer (save-window-excursion
                    (vterm vterm-buffer-name))))
      ;; Check if vterm successfully created a buffer
      (unless buffer
        (error "Failed to create vterm buffer.  Please ensure vterm is properly installed"))
      ;; Get the process that vterm created
      (let ((process (get-buffer-process buffer)))
        (unless process
          (error "Failed to get vterm process.  The vterm buffer may not have initialized properly"))
        ;; Check if buffer is still alive
        (unless (buffer-live-p buffer)
          (error "Vterm buffer was killed during initialization"))
        (cons buffer process)))))

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
  (claude-code-ide--cleanup-dead-workspace-sessions)
  (let ((sessions '()))
    (maphash (lambda (workspace-name session)
               (when (and (plist-get session :process)
                          (process-live-p (plist-get session :process)))
                 (let ((directory (plist-get session :directory)))
                   (push (cons (format "%s [%s]" workspace-name (abbreviate-file-name directory))
                               workspace-name)
                         sessions))))
             claude-code-ide--workspace-sessions)
    (if sessions
        (let ((choice (completing-read "Switch to Claude Code session: "
                                       sessions nil t)))
          (when choice
            (let* ((workspace-name (alist-get choice sessions nil nil #'string=))
                   (session (claude-code-ide--get-workspace-session workspace-name))
                   (buffer (when session (plist-get session :buffer))))
              (if (and buffer (buffer-live-p buffer))
                  (claude-code-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (claude-code-ide-log "No active Claude Code sessions"))))

;;;###autoload
(defun claude-code-ide-insert-at-mentioned ()
  "Insert selected text into Claude prompt."
  (interactive)
  (progn
    (claude-code-ide-mcp-send-at-mentioned)
    (claude-code-ide-debug "Sent selection to Claude Code")))


;;;###autoload
(defun claude-code-ide-insert-newline ()
  "Send newline (backslash + return) to vterm buffer.
This simulates typing backslash followed by Enter, which Claude Code interprets as a newline."
  (interactive)
  (vterm-send-string "\\")
  (vterm-send-return))

;;; Workspace Hook Management

(defun claude-code-ide--enable-workspace-hooks ()
  "Enable workspace lifecycle hooks for automatic session switching.
Only registers hooks if persp-mode functions are available."
  (when (and (featurep 'persp-mode)
             (fboundp 'persp-name)
             (boundp 'persp-activated-functions)
             (boundp 'persp-before-deactivate-functions))
    (add-hook 'persp-activated-functions #'claude-code-ide--workspace-activated-h)
    (add-hook 'persp-before-deactivate-functions #'claude-code-ide--workspace-deactivated-h)
    (claude-code-ide-debug "Workspace lifecycle hooks enabled")))

(defun claude-code-ide--disable-workspace-hooks ()
  "Disable workspace lifecycle hooks."
  (remove-hook 'persp-activated-functions #'claude-code-ide--workspace-activated-h)
  (remove-hook 'persp-before-deactivate-functions #'claude-code-ide--workspace-deactivated-h)
  (claude-code-ide-debug "Workspace lifecycle hooks disabled"))

;; Auto-enable hooks when module loads if persp-mode is available
(when (featurep 'persp-mode)
  (claude-code-ide--enable-workspace-hooks))

;; Enable hooks when persp-mode is loaded later
(eval-after-load 'persp-mode
  '(claude-code-ide--enable-workspace-hooks))

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
  (when-let ((session (claude-code-ide--get-workspace-session workspace-name)))
    (let ((process (plist-get session :process))
          (directory (plist-get session :directory)))
      (when (and process (process-live-p process))
        (claude-code-ide--cleanup-on-exit directory)
        (claude-code-ide-log "Claude Code session killed for workspace: %s" workspace-name)))))

(defun claude-code-ide--restart-workspace-session ()
  "Restart Claude Code session in current workspace."
  (let ((workspace-name (claude-code-ide--get-workspace-name)))
    (claude-code-ide--kill-workspace-session-internal workspace-name)
    ;; Small delay to ensure cleanup completes
    (run-with-timer 0.1 nil
                    (lambda ()
                      (claude-code-ide--start-session)
                      (claude-code-ide-log "Claude Code session restarted for workspace: %s" workspace-name)))))

(defun claude-code-ide--switch-workspace-sessions ()
  "Switch between active Claude Code workspace sessions."
  (let ((active-sessions '()))
    (maphash (lambda (workspace-name _session)
               (when (claude-code-ide-mcp--get-session-for-workspace workspace-name)
                 (push (cons workspace-name workspace-name) active-sessions)))
             claude-code-ide-mcp--sessions)
    (if active-sessions
        (let ((choice (completing-read "Switch to Claude workspace session: "
                                       active-sessions nil t)))
          (when choice
            (claude-code-ide--switch-to-workspace-session choice)))
      (claude-code-ide-log "No active Claude Code workspace sessions"))))

;;; Workspace Persistence Integration

(defun claude-code-ide--serialize-session-state (session)
  "Serialize Claude session state for workspace persistence.
SESSION is a session plist containing :process, :directory, :buffer, etc.
Returns a serializable plist without process objects."
  (when session
    (list :directory (plist-get session :directory)
          :workspace-name (plist-get session :workspace-name)
          :created (plist-get session :created)
          :last-active (plist-get session :last-active)
          :buffer-name (when-let ((buffer (plist-get session :buffer)))
                         (and (buffer-live-p buffer) (buffer-name buffer)))
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
              (buffer-name (plist-get state :buffer-name))
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
    (if-let ((existing-session (claude-code-ide--get-workspace-session workspace-name)))
        (if (and (plist-get existing-session :process)
                 (process-live-p (plist-get existing-session :process)))
            ;; Switch to existing session
            (claude-code-ide--switch-to-workspace-session workspace-name)
          ;; Session exists but process is dead - try restoration first
          (unless (claude-code-ide--restore-session-on-demand workspace-name)
            ;; Restoration failed or no data - create new session
            (claude-code-ide--start-session)))
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
(when (featurep 'persp-mode)
  (claude-code-ide--register-persistence-hooks))

;;; VTerm Integration for Unmanaged Terminals

(defun claude-code-ide--setup-vterm-environment ()
  "Set up workspace-specific environment variables for vterm sessions.
This hook runs on vterm-mode-hook to automatically configure environment
variables for unmanaged vterm sessions (not created by claude-code-ide)."
  (when (and (or (and (boundp 'vterm-mode) vterm-mode)
                 (bound-and-true-p vterm-mode))
             ;; Only apply to unmanaged vterm sessions
             (not (string-match-p "\\*claude-code" (buffer-name))))
    (let* ((workspace (claude-code-ide--get-workspace-name))
           (port (when workspace
                   (claude-code-ide-mcp-get-workspace-port workspace))))
      (when port
        (claude-code-ide-debug "Setting up vterm environment for workspace %s (port %d)" workspace port)
        ;; Set buffer-local vterm-environment
        (make-local-variable 'vterm-environment)
        (setq vterm-environment
              (append (list (format "CLAUDE_CODE_SSE_PORT=%d" port)
                            "ENABLE_IDE_INTEGRATION=true"
                            "TERM_PROGRAM=emacs"
                            "FORCE_CODE_TERMINAL=true")
                      ;; Preserve existing environment variables
                      (when (boundp 'vterm-environment) vterm-environment)))
        (claude-code-ide-debug "Vterm environment configured: %s" vterm-environment)))))

(defun claude-code-ide--enable-vterm-integration ()
  "Enable automatic vterm integration for unmanaged terminals."
  (when (fboundp 'vterm-mode)
    (add-hook 'vterm-mode-hook #'claude-code-ide--setup-vterm-environment)))

(defun claude-code-ide--disable-vterm-integration ()
  "Disable automatic vterm integration."
  (when (fboundp 'vterm-mode)
    (remove-hook 'vterm-mode-hook #'claude-code-ide--setup-vterm-environment)))

;; Enable vterm integration by default
(claude-code-ide--enable-vterm-integration)

;;; Doom Keybindings

;; Only define keybindings if we're in a Doom environment
(when (and (featurep 'doom) (fboundp 'map!))
  (map! :leader
        (:prefix ("TAB" . "workspace")
                 :desc "Claude Code session" "c" #'+workspace/claude-code
                 :desc "Kill Claude session" "C" #'+workspace/claude-code-kill
                 :desc "Restart Claude session" "r" #'+workspace/claude-code-restart
                 :desc "Switch Claude sessions" "s" #'+workspace/claude-code-switch)))

(provide 'claude-code-ide)

;;; claude-code-ide.el ends here
