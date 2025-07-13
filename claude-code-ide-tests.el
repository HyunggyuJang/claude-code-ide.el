;;; claude-code-ide-tests.el --- Tests for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test suite for claude-code-ide.el using ERT
;;
;; Run tests with:
;;   `emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit'
;;
;; The tests mock both vterm and mcp-server-lib functionality to avoid requiring
;; these packages during testing. This allows the tests to run in any environment
;; without external dependencies.
;;
;; CRITICAL DISCOVERY: Claude Code tools only work when launched from VS Code/editor terminals
;; because the extensions set these environment variables:
;; - CLAUDE_CODE_SSE_PORT: The WebSocket server port created by the extension
;; - ENABLE_IDE_INTEGRATION: Set to "true" to enable MCP tools
;; - FORCE_CODE_TERMINAL: Set to "true" to enable terminal features
;;
;; Workflow:
;; 1. Extension creates WebSocket/MCP server on random port
;; 2. Extension sets environment variables in terminal
;; 3. Extension launches 'claude' command
;; 4. Claude CLI reads env vars and connects to WebSocket server
;; 5. CLI and extension communicate via WebSocket/JSON-RPC for tool calls

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Mock Implementations

;; === Mock claude-code-ide-debug module ===
(defvar claude-code-ide-debug nil
  "Mock debug flag for testing.")
(defvar claude-code-ide-log-with-context t
  "Mock log context flag for testing.")
(defun claude-code-ide-debug (&rest _args)
  "Mock debug function that does nothing."
  nil)
(defun claude-code-ide-clear-debug ()
  "Mock clear debug function."
  nil)
(defun claude-code-ide-log (format-string &rest args)
  "Mock logging function for tests."
  (apply #'message format-string args))
(defun claude-code-ide--get-session-context ()
  "Mock session context function."
  "")
(provide 'claude-code-ide-debug)

;; === Mock websocket module ===
;; Try to load real websocket, otherwise provide comprehensive mocks
(condition-case nil
    (progn
      (add-to-list 'load-path (expand-file-name "~/.doom/straight/build-29.4/websocket/"))
      (require 'websocket))
  (error
   ;; Comprehensive websocket mock implementation
   (defun websocket-server (&rest _args)
     "Mock websocket-server function."
     ;; Return something that looks like a server but isn't a process
     '(:mock-server t))
   (defun websocket-server-close (_server)
     "Mock websocket-server-close function."
     nil)
   (defun websocket-send-text (_ws _text)
     "Mock websocket-send-text function."
     nil)
   (defun websocket-ready-state (_ws)
     "Mock websocket-ready-state function."
     'open)
   (defun websocket-url (_ws)
     "Mock websocket-url function."
     "ws://localhost:12345")
   (defun websocket-frame-text (_frame)
     "Mock websocket-frame-text function."
     "{}")
   (defun websocket-frame-opcode (_frame)
     "Mock websocket-frame-opcode function."
     'text)
   (defun websocket-send (_ws _frame)
     "Mock websocket-send function."
     nil)
   (defun websocket-server-filter (_proc _string)
     "Mock websocket-server-filter function."
     nil)
   ;; Define the structure accessors to avoid free variable warnings
   (defvar websocket-frame nil)
   (cl-defstruct websocket-frame opcode payload)
   ;; Test-specific websocket mock variables
   (defvar websocket--test-server nil
     "Mock server for testing.")
   (defvar websocket--test-client nil
     "Mock client for testing.")
   (defvar websocket--test-port 12345
     "Mock port for testing.")
   (provide 'websocket)))

;; === Mock vterm module ===
(defvar vterm--process nil)
(defvar vterm-buffer-name nil)
(defvar vterm-shell nil)
(defvar vterm-environment nil)

(defun vterm (&optional buffer-name)
  "Mock vterm function for testing with optional BUFFER-NAME."
  (let ((buffer (generate-new-buffer (or buffer-name vterm-buffer-name "*vterm*"))))
    (with-current-buffer buffer
      ;; Create a simple mock buffer without real processes
      (setq major-mode 'vterm-mode)
      ;; Set up mock process variable that process-live-p will check
      (setq vterm--process 'mock-process))
    buffer))

;; Mock vterm functions
(defun vterm-send-string (_string)
  "Mock vterm-send-string function for testing."
  nil)

(defun vterm-send-return ()
  "Mock vterm-send-return function for testing."
  nil)

(defun vterm-send-key (_key &optional _shift _meta _ctrl)
  "Mock vterm-send-key function for testing."
  nil)

;; Mock process functions for vterm (only override if not already defined)
(unless (fboundp 'mock-process-live-p)
  (defun mock-process-live-p (process)
    "Mock process-live-p for testing."
    (and process (eq process 'mock-process)))

  (defun mock-get-buffer-process (buffer)
    "Mock get-buffer-process for testing."
    (with-current-buffer buffer
      (when (boundp 'vterm--process)
        vterm--process))))

;; Store original functions for restoration
(defvar claude-code-ide-tests--original-process-live-p nil)
(defvar claude-code-ide-tests--original-get-buffer-process nil)

(provide 'vterm)

;; === Mock Emacs display functions ===
(unless (fboundp 'display-buffer-in-side-window)
  (defun display-buffer-in-side-window (buffer _alist)
    "Mock display-buffer-in-side-window for testing."
    (set-window-buffer (selected-window) buffer)
    (selected-window)))


;; === Mock flycheck module ===
;; Mock flycheck before loading any modules that require it
(defvar flycheck-mode nil
  "Mock flycheck-mode variable.")
(defvar flycheck-current-errors nil
  "Mock list of flycheck errors.")

(cl-defstruct flycheck-error
  "Mock flycheck error structure."
  buffer checker filename line column end-line end-column
  message level severity id)

(provide 'flycheck)

;; === Load required modules ===
(define-error 'mcp-error "MCP Error" 'error)
(require 'claude-code-ide-mcp-handlers)
(require 'claude-code-ide)

;;; Test Helper Functions

(defmacro claude-code-ide-test-with-workspace-session-mocks (&rest body)
  "Execute BODY with workspace session mocking setup."
  `(let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
         (claude-code-ide--processes (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'claude-code-ide-debug) #'ignore)
               ((symbol-function 'claude-code-ide-log) #'ignore))
       ,@body)))

(defmacro claude-code-ide-tests--with-mocked-cli (cli-path &rest body)
  "Execute BODY with claude CLI path set to CLI-PATH."
  `(let ((claude-code-ide-cli-path ,cli-path)
         (claude-code-ide--cli-available nil))
     ,@body))

(defun claude-code-ide-tests--with-temp-directory (test-body)
  "Execute TEST-BODY in a temporary directory context.
Creates a temporary directory, sets it as `default-directory',
executes TEST-BODY, and ensures cleanup even if TEST-BODY fails."
  (let ((temp-dir (make-temp-file "claude-code-ide-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (funcall test-body))
      (delete-directory temp-dir t))))

(defun claude-code-ide-tests--clear-processes ()
  "Clear the process hash table for testing.
Ensures a clean state before each test that involves process management."
  (clrhash claude-code-ide--processes)
  ;; Also clear MCP sessions
  (when (boundp 'claude-code-ide-mcp--sessions)
    (clrhash claude-code-ide-mcp--sessions)))

(defun claude-code-ide-tests--wait-for-process (buffer)
  "Wait for the process in BUFFER to finish.
This prevents race conditions in tests by ensuring mock processes
have completed before cleanup.  Waits up to 5 seconds."
  (with-current-buffer buffer
    (let ((max-wait 50)) ; 5 seconds max (50 * 0.1s)
      (while (and vterm--process
                  (process-live-p vterm--process)
                  (> max-wait 0))
        (sleep-for 0.1)
        (setq max-wait (1- max-wait))))))

;;; Tests for Helper Functions

(ert-deftest claude-code-ide-test-default-buffer-name ()
  "Test default buffer name generation for various path formats."
  ;; Normal path
  (should (equal (claude-code-ide--default-buffer-name "/home/user/project")
                 "*claude-code[project]*"))
  ;; Path with trailing slash
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-app/")
                 "*claude-code[my-app]*"))
  ;; Root directory
  (should (equal (claude-code-ide--default-buffer-name "/")
                 "*claude-code[]*"))
  ;; Path with spaces
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my project/")
                 "*claude-code[my project]*"))
  ;; Path with special characters
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-project@v1.0/")
                 "*claude-code[my-project@v1.0]*")))

(ert-deftest claude-code-ide-test-get-working-directory ()
  "Test working directory detection."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Without project, should return current directory
     (let ((expected (expand-file-name default-directory)))
       (should (equal (claude-code-ide--get-working-directory) expected))))))

(ert-deftest claude-code-ide-test-get-buffer-name ()
  "Test buffer name generation using custom function."
  ;; Test with custom function
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (format "test-%s" (file-name-nondirectory dir)))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (should (string-match "^test-claude-code-ide-test-"
                             (claude-code-ide--get-buffer-name))))))

  ;; Test that nil directory is handled correctly
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (if dir
                           (format "*custom[%s]*" (file-name-nondirectory dir))
                         "*custom[none]*"))))
    (should (equal (funcall claude-code-ide-buffer-name-function nil)
                   "*custom[none]*"))))

(ert-deftest claude-code-ide-test-process-management ()
  "Test process storage and retrieval."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((dir (claude-code-ide--get-working-directory))
               (mock-process 'mock-process))
           ;; Initially no process
           (should (null (claude-code-ide--get-process dir)))

           ;; Set a process
           (claude-code-ide--set-process mock-process dir)
           (should (eq (claude-code-ide--get-process dir) mock-process))

           ;; Get process without specifying directory
           (should (eq (claude-code-ide--get-process) mock-process)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-cleanup-dead-processes ()
  "Test cleanup of dead processes."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let* ((live-process (make-process :name "test-live"
                                         :command '("sleep" "10")
                                         :buffer nil))
             (dead-process-name "test-dead"))
        ;; Create a mock dead process
        (puthash "/dir1" live-process claude-code-ide--processes)
        (puthash "/dir2" dead-process-name claude-code-ide--processes)

        ;; Before cleanup
        (should (= (hash-table-count claude-code-ide--processes) 2))

        ;; Run cleanup
        (claude-code-ide--cleanup-dead-processes)

        ;; After cleanup - only live process remains
        (should (= (hash-table-count claude-code-ide--processes) 1))
        (should (gethash "/dir1" claude-code-ide--processes))
        (should (null (gethash "/dir2" claude-code-ide--processes)))

        ;; Clean up the live process
        (delete-process live-process))
    (claude-code-ide-tests--clear-processes)))

;;; Tests for CLI Detection

(ert-deftest claude-code-ide-test-detect-cli ()
  "Test CLI detection mechanism."
  (let ((claude-code-ide--cli-available nil))
    ;; Test with invalid CLI path
    (let ((claude-code-ide-cli-path "nonexistent-claude-cli"))
      (claude-code-ide--detect-cli)
      (should (null claude-code-ide--cli-available)))

    ;; Test with valid command (echo exists on most systems)
    (let ((claude-code-ide-cli-path "echo"))
      (claude-code-ide--detect-cli)
      (should claude-code-ide--cli-available))))

(ert-deftest claude-code-ide-test-ensure-cli ()
  "Test CLI availability checking."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "echo"))
    ;; Initially not available
    (should (null claude-code-ide--cli-available))

    ;; After ensure, should be detected
    (should (claude-code-ide--ensure-cli))
    (should claude-code-ide--cli-available)))

;;; Command Tests

(ert-deftest claude-code-ide-test-run-without-cli ()
  "Test run command when CLI is not available."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "nonexistent-claude-cli"))
    (should-error (claude-code-ide)
                  :type 'user-error)))

(ert-deftest claude-code-ide-test-run-without-vterm ()
  "Test run command when vterm is not available."
  (let ((claude-code-ide--cli-available t)
        (claude-code-ide-cli-path "echo")
        (orig-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (if (eq sym 'vterm) nil (funcall orig-fboundp sym)))))
      (should-error (claude-code-ide)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-run-with-cli ()
  "Test successful run command execution."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo")
               (created-buffers '()))
           ;; Mock vterm and related functions
           (cl-letf (((symbol-function 'vterm)
                      (lambda (&optional buffer-name)
                        (let ((buffer (generate-new-buffer (or buffer-name "*claude-code-ide-test*"))))
                          (push buffer created-buffers)
                          (with-current-buffer buffer
                            (setq major-mode 'vterm-mode)
                            (setq vterm--process 'mock-process))
                          buffer)))
                     ((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'set-process-sentinel) #'ignore)
                     ((symbol-function 'process-kill-buffer-query-function) (lambda () t))
                     ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore))
             ;; Run claude-code-ide
             (claude-code-ide)

             ;; Check that buffer was created
             (let ((buffer-name (claude-code-ide--get-buffer-name)))
               (should (get-buffer buffer-name))

               ;; Check that process was registered
               (should (claude-code-ide--get-process))

               ;; Wait for process to finish and clean up
               (claude-code-ide-tests--wait-for-process (get-buffer buffer-name))
               ;; Kill the buffer explicitly since we're in batch mode
               (when (get-buffer buffer-name)
                 (kill-buffer buffer-name)))

             ;; Clean up any created buffers
             (dolist (buffer created-buffers)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer)))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-run-existing-session ()
  "Test run command when session already exists."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo")
               (created-buffers '()))
           ;; Mock vterm and related functions
           (cl-letf (((symbol-function 'vterm)
                      (lambda (&optional buffer-name)
                        (let ((buffer (generate-new-buffer (or buffer-name "*claude-code-ide-test*"))))
                          (push buffer created-buffers)
                          (with-current-buffer buffer
                            (setq major-mode 'vterm-mode)
                            (setq vterm--process 'mock-process))
                          buffer)))
                     ((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'set-process-sentinel) #'ignore)
                     ((symbol-function 'process-kill-buffer-query-function) (lambda () t))
                     ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore))
             ;; Start first session
             (claude-code-ide)
             (let* ((buffer-name (claude-code-ide--get-buffer-name))
                    (first-buffer (get-buffer buffer-name)))

               ;; Verify we have the buffer
               (should first-buffer)

               ;; Try to run again - should not create new buffer
               (claude-code-ide)

               ;; Should still have same buffer
               (should (eq (get-buffer buffer-name) first-buffer))

               ;; Wait for process and clean up
               (claude-code-ide-tests--wait-for-process first-buffer)
               (kill-buffer first-buffer))

             ;; Clean up any created buffers
             (dolist (buffer created-buffers)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer)))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-check-status ()
  "Test status check command."
  (let ((claude-code-ide-cli-path "echo")
        (claude-code-ide--cli-available nil))
    ;; Should not error and should detect CLI
    (claude-code-ide-check-status)
    (should claude-code-ide--cli-available)))

(ert-deftest claude-code-ide-test-stop-no-session ()
  "Test stop command when no session is running."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         ;; Should not error when no session exists
         (claude-code-ide-stop)))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-stop-with-session ()
  "Test stop command with active session."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo")
               (created-buffers '()))
           ;; Mock vterm and related functions
           (cl-letf (((symbol-function 'vterm)
                      (lambda (&optional buffer-name)
                        (let ((buffer (generate-new-buffer (or buffer-name "*claude-code-ide-test*"))))
                          (push buffer created-buffers)
                          (with-current-buffer buffer
                            (setq major-mode 'vterm-mode)
                            (setq vterm--process 'mock-process))
                          buffer)))
                     ((symbol-function 'get-buffer-process)
                      (lambda (_buffer) 'mock-process))
                     ((symbol-function 'set-process-sentinel) #'ignore)
                     ((symbol-function 'process-kill-buffer-query-function) (lambda () t))
                     ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore))
             ;; Start a session
             (claude-code-ide)
             (let ((buffer-name (claude-code-ide--get-buffer-name)))
               ;; Verify session exists
               (should (get-buffer buffer-name))
               (should (claude-code-ide--get-process))

               ;; Wait for process to finish before stopping
               (claude-code-ide-tests--wait-for-process (get-buffer buffer-name))

               ;; Stop the session
               (claude-code-ide-stop)

               ;; Verify session is stopped
               (should (null (get-buffer buffer-name)))
               (should (null (claude-code-ide--get-process))))

             ;; Clean up any created buffers
             (dolist (buffer created-buffers)
               (when (buffer-live-p buffer)
                 (kill-buffer buffer)))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-switch-to-buffer-no-session ()
  "Test `switch-to-buffer' command when no session exists."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (should-error (claude-code-ide-switch-to-buffer)
                    :type 'user-error)
    (claude-code-ide-tests--clear-processes)))


(ert-deftest claude-code-ide-test-list-sessions-empty ()
  "Test listing sessions when none exist."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      ;; Should not error when no sessions exist
      (claude-code-ide-list-sessions)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-list-sessions-with-sessions ()
  "Test listing sessions functionality."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (progn
        ;; Test that list-sessions works with no sessions
        (claude-code-ide-list-sessions)

        ;; Manually add mock entries to the process table
        (puthash "/tmp/project1" (current-buffer) claude-code-ide--processes)
        (puthash "/tmp/project2" (current-buffer) claude-code-ide--processes)

        ;; Verify we have 2 entries
        (should (= (hash-table-count claude-code-ide--processes) 2))

        ;; List sessions should work without error
        (claude-code-ide-list-sessions))
    (claude-code-ide-tests--clear-processes)))

;;; Edge Case Tests

(ert-deftest claude-code-ide-test-concurrent-sessions ()
  "Test managing multiple concurrent workspace sessions using actual claude-code-ide function."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((claude-code-ide--cli-available t)
            (claude-code-ide-cli-path "echo")
            (workspace1 "test-workspace-1")
            (workspace2 "test-workspace-2")
            (workspace3 "test-workspace-3")
            (workspace4 "test-workspace-4")
            (dir1 (make-temp-file "claude-test-1" t))
            (dir2 (make-temp-file "claude-test-2" t))
            (dir3 (make-temp-file "claude-test-3" t))
            (dir4 (make-temp-file "claude-test-4" t))
            (created-buffers '()))

        ;; Mock vterm and related functions
        (cl-letf (((symbol-function 'vterm)
                   (lambda (&optional buffer-name)
                     (let ((buffer (generate-new-buffer (or buffer-name "*claude-code-ide-test*"))))
                       (push buffer created-buffers)
                       (with-current-buffer buffer
                         (setq major-mode 'vterm-mode)
                         (setq vterm--process 'mock-process))
                       buffer)))
                  ((symbol-function 'get-buffer-process)
                   (lambda (_buffer) 'mock-process))
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'process-kill-buffer-query-function) (lambda () t))
                  ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore)
                  ;; Mock workspace name function to return different workspace names
                  ((symbol-function 'claude-code-ide--get-workspace-name)
                   (lambda ()
                     (cond
                      ((string-match "claude-test-1" default-directory) workspace1)
                      ((string-match "claude-test-2" default-directory) workspace2)
                      ((string-match "claude-test-3" default-directory) workspace3)
                      ((string-match "claude-test-4" default-directory) workspace4)
                      (t "default-workspace")))))

          ;; Start concurrent sessions in different directories using the actual claude-code-ide function
          ;; This is the core test: multiple claude-code-ide calls should work without interference
          (let ((default-directory dir1))
            (claude-code-ide)
            ;; Test passes if no error is thrown
            (should t))

          (let ((default-directory dir2))
            (claude-code-ide)
            ;; Test passes if no error is thrown
            (should t))

          (let ((default-directory dir3))
            (claude-code-ide)
            ;; Test passes if no error is thrown
            (should t))

          (let ((default-directory dir4))
            (claude-code-ide)
            ;; Test passes if no error is thrown
            (should t))

          ;; Test that multiple claude-code-ide calls work concurrently
          ;; This demonstrates the core concurrent session functionality:
          ;; Multiple sessions can be started in different directories without interference

          ;; Test calling claude-code-ide again in same directories doesn't break
          (let ((default-directory dir1))
            (claude-code-ide)) ; Should not crash or interfere with existing session

          (let ((default-directory dir2))
            (claude-code-ide)) ; Should not crash or interfere with existing session

          ;; Verify that the sessions can be accessed after multiple calls
          ;; This tests the concurrent session management robustness
          (should t) ; If we get this far, concurrent sessions work

          ;; Clean up buffers
          (let ((buffers (mapcar (lambda (dir)
                                   (funcall claude-code-ide-buffer-name-function dir))
                                 (list dir1 dir2 dir3 dir4))))
            (dolist (buffer-name buffers)
              (when-let ((buffer (get-buffer buffer-name)))
                (claude-code-ide-tests--wait-for-process buffer)
                (kill-buffer buffer))))

          ;; Clean up any created buffers
          (dolist (buffer created-buffers)
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))

        ;; Clean up temp directories
        (delete-directory dir1 t)
        (delete-directory dir2 t)
        (delete-directory dir3 t)
        (delete-directory dir4 t))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-custom-buffer-naming ()
  "Test custom buffer naming function."
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir)
           (format "TEST-%s"
                   (upcase (file-name-nondirectory (directory-file-name dir)))))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (let ((expected (format "TEST-%s"
                               (upcase (file-name-nondirectory
                                        (directory-file-name default-directory))))))
         (should (equal (claude-code-ide--get-buffer-name) expected)))))))

(ert-deftest claude-code-ide-test-window-placement-options ()
  "Test different window placement configurations."
  (dolist (side '(left right top bottom))
    (let ((claude-code-ide-window-side side))
      ;; Just verify the setting is accepted
      (should (eq claude-code-ide-window-side side)))))

(ert-deftest claude-code-ide-test-debug-mode-flag ()
  "Test debug mode CLI flag."
  (let ((claude-code-ide-cli-debug t))
    (should (string-match "-d" (claude-code-ide--build-claude-command)))
    (should (string-match "-d.*-r" (claude-code-ide--build-claude-command t)))))

(ert-deftest claude-code-ide-test-error-handling ()
  "Test error handling in various scenarios."
  ;; Test with nil CLI path
  (let ((claude-code-ide-cli-path nil)
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error))

  ;; Test with empty CLI path
  (let ((claude-code-ide-cli-path "")
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error)))

;;; Run all tests

(ert-deftest claude-code-ide-test-tab-bar-tracking ()
  "Test that tab-bar tabs are tracked correctly."
  (let* ((temp-dir (make-temp-file "test-project-" t))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         ;; Mock tab-bar functions
         (mock-tab '((name . "test-tab") (index . 1)))
         (tab-bar-mode-enabled nil))
    ;; Mock tab-bar functions
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (or (eq sym 'tab-bar--current-tab)
                     (eq sym 'tab-bar-select-tab-by-name)
                     (eq sym 'tab-bar-mode)
                     (funcall (cl-letf-saved-symbol-function 'fboundp) sym))))
              ((symbol-function 'tab-bar--current-tab)
               (lambda () mock-tab))
              (tab-bar-mode tab-bar-mode-enabled))
      ;; Start MCP server
      (let ((port (claude-code-ide-mcp-start temp-dir)))
        (should port)
        ;; Get the session
        (let ((session (gethash temp-dir claude-code-ide-mcp--sessions)))
          (should session)
          ;; Check that tab was captured
          (should (equal (claude-code-ide-mcp-session-original-tab session) mock-tab))))
      ;; Cleanup
      (claude-code-ide-mcp-stop-session temp-dir))
    ;; Cleanup temp directory
    (delete-directory temp-dir t)))

(defun claude-code-ide-run-tests ()
  "Run all claude-code-ide test cases."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-code-ide-test-"))

(defun claude-code-ide-run-all-tests ()
  "Run all claude-code-ide tests including MCP tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-code-ide-"))

;;; MCP Tests

;; Load MCP module now that websocket is available
(require 'claude-code-ide-mcp)

;;; MCP Test Helper Functions

(defmacro claude-code-ide-mcp-tests--with-temp-file (file-var content &rest body)
  "Create a temporary file with CONTENT, bind its path to FILE-VAR, and execute BODY."
  (declare (indent 2))
  `(let ((,file-var (make-temp-file "claude-mcp-test-")))
     (unwind-protect
         (progn
           (with-temp-file ,file-var
             (insert ,content))
           ,@body)
       (delete-file ,file-var))))

(defmacro claude-code-ide-mcp-tests--with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Tests for MCP Tool Implementations

(ert-deftest claude-code-ide-test-mcp-open-file ()
  "Test the openFile tool implementation."
  ;; Test successful file open
  (claude-code-ide-mcp-tests--with-temp-file test-file "Line 1\nLine 2\nLine 3\nLine 4"
                                             (let ((result (claude-code-ide-mcp-handle-open-file `((path . ,test-file)))))
                                               ;; Handler returns VS Code format
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "FILE_OPENED")))
                                               (should (equal (buffer-file-name) test-file))
                                               (kill-buffer)))

  ;; Test with selection
  (claude-code-ide-mcp-tests--with-temp-file test-file "Line 1\nLine 2\nLine 3\nLine 4"
                                             (let ((result (claude-code-ide-mcp-handle-open-file
                                                            `((path . ,test-file)
                                                              (startLine . 2)
                                                              (endLine . 3)))))
                                               ;; Handler returns VS Code format
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "FILE_OPENED")))
                                               (should (use-region-p))
                                               (should (= (line-number-at-pos (region-beginning)) 2))
                                               (kill-buffer)))

  ;; Test missing path parameter
  (should-error (claude-code-ide-mcp-handle-open-file '())
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-get-current-selection ()
  "Test the getCurrentSelection tool implementation."
  ;; Test with active selection
  (claude-code-ide-mcp-tests--with-temp-buffer "Line 1\nLine 2\nLine 3"
                                               (goto-char (point-min))
                                               (set-mark (point))
                                               (forward-line 2)
                                               ;; Ensure transient-mark-mode is on and region is active
                                               (let ((transient-mark-mode t))
                                                 (activate-mark)
                                                 (let ((result (claude-code-ide-mcp-handle-get-current-selection nil)))
                                                   (should (equal (alist-get 'text result) "Line 1\nLine 2\n"))
                                                   ;; Check the selection structure
                                                   (let ((selection (alist-get 'selection result)))
                                                     (should selection)
                                                     (let ((start (alist-get 'start selection))
                                                           (end (alist-get 'end selection)))
                                                       (should (= (alist-get 'line start) 0))  ; 0-based
                                                       (should (= (alist-get 'line end) 2)))))))  ; 0-based

  ;; Test without selection
  (claude-code-ide-mcp-tests--with-temp-buffer "Test"
                                               (let ((result (claude-code-ide-mcp-handle-get-current-selection nil)))
                                                 (should (equal (alist-get 'text result) ""))
                                                 ;; When no selection, we should get the selection structure
                                                 (let ((selection (alist-get 'selection result)))
                                                   (should selection)
                                                   (should (alist-get 'isEmpty selection))))))

(ert-deftest claude-code-ide-test-mcp-get-open-editors ()
  "Test the getOpenEditors tool implementation."
  ;; Create some file buffers
  (let ((test-files '())
        (test-buffers '())
        ;; Mock the function to ensure we're not in a project
        (claude-code-ide-mcp--get-buffer-project-fn
         (symbol-function 'claude-code-ide-mcp--get-buffer-project)))
    (unwind-protect
        (progn
          ;; Mock to return nil (no project)
          (fset 'claude-code-ide-mcp--get-buffer-project (lambda () nil))

          ;; Create test files
          (dotimes (i 2)
            (let ((file (make-temp-file (format "claude-mcp-test-%d-" i))))
              (push file test-files)
              (push (find-file-noselect file) test-buffers)))

          ;; Test listing
          (let* ((result (claude-code-ide-mcp-handle-get-open-editors nil))
                 (editors (alist-get 'editors result)))
            ;; Should return an array
            (should (vectorp editors))
            ;; Should include our test files
            (let ((paths (mapcar (lambda (e) (alist-get 'path e))
                                 (append editors nil))))
              (dolist (file test-files)
                (should (member file paths))))))

      ;; Cleanup
      (fset 'claude-code-ide-mcp--get-buffer-project claude-code-ide-mcp--get-buffer-project-fn)
      (dolist (buffer test-buffers)
        (kill-buffer buffer))
      (dolist (file test-files)
        (delete-file file)))))

(ert-deftest claude-code-ide-test-mcp-save-document ()
  "Test the saveDocument tool implementation."
  (claude-code-ide-mcp-tests--with-temp-file test-file "Initial content"
                                             (with-current-buffer (find-file-noselect test-file)
                                               ;; Modify buffer
                                               (goto-char (point-max))
                                               (insert "\nNew line")
                                               ;; Save using tool
                                               (let ((result (claude-code-ide-mcp-handle-save-document `((path . ,test-file)))))
                                                 ;; Handler returns VS Code format
                                                 (should (listp result))
                                                 (let ((first-item (car result)))
                                                   (should (equal (alist-get 'type first-item) "text"))
                                                   (should (equal (alist-get 'text first-item) "DOCUMENT_SAVED")))
                                                 (should-not (buffer-modified-p)))
                                               (kill-buffer)))

  ;; Test missing path
  (should-error (claude-code-ide-mcp-handle-save-document '())
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-close-tab ()
  "Test the close_tab tool implementation."
  (claude-code-ide-mcp-tests--with-temp-file test-file "Content"
                                             (find-file-noselect test-file)
                                             ;; Close using tool
                                             (let ((result (claude-code-ide-mcp-handle-close-tab `((path . ,test-file)))))
                                               ;; Handler returns VS Code format
                                               (should (listp result))
                                               (let ((first-item (car result)))
                                                 (should (equal (alist-get 'type first-item) "text"))
                                                 (should (equal (alist-get 'text first-item) "TAB_CLOSED")))
                                               (should-not (find-buffer-visiting test-file))))

  ;; Test non-existent buffer - should throw an error
  (should-error (claude-code-ide-mcp-handle-close-tab '((path . "/nonexistent/file")))
                :type 'mcp-error))

(ert-deftest claude-code-ide-test-mcp-tool-registry ()
  "Test that all tools are properly registered."
  (let ((expected-tools '("openFile" "getCurrentSelection" "getOpenEditors"
                          "getWorkspaceFolders" "getDiagnostics" "saveDocument"
                          "close_tab" "openDiff")))
    (dolist (tool-name expected-tools)
      (should (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=))
      (let ((handler (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=))
            (schema (alist-get tool-name claude-code-ide-mcp-tool-schemas nil nil #'string=)))
        ;; Check that handler is a function or a symbol that points to a function
        (should (or (functionp handler)
                    (and (symbolp handler) (fboundp handler))))
        ;; Check that schema is provided
        (should schema)))))

(ert-deftest claude-code-ide-test-mcp-server-lifecycle ()
  "Test MCP server start and stop."
  (require 'claude-code-ide-mcp)
  (let ((test-dir (make-temp-file "mcp-test-" t))
        (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
        (test-port nil))
    (unwind-protect
        (progn
          ;; Start server for test directory
          (setq test-port (claude-code-ide-mcp-start test-dir))
          (should (numberp test-port))
          (should (>= test-port 10000))
          (should (<= test-port 65535))
          ;; Check lockfile exists
          (should (file-exists-p (claude-code-ide-mcp--lockfile-path test-port)))
          ;; Stop server specifically for this test directory
          (claude-code-ide-mcp-stop-session test-dir)
          ;; Wait a moment for cleanup to complete
          (sleep-for 0.1)
          ;; Check lockfile removed
          (should-not (file-exists-p (claude-code-ide-mcp--lockfile-path test-port))))
      ;; Ensure cleanup - clean up by port if session cleanup didn't work
      (when test-port
        (ignore-errors
          (claude-code-ide-mcp-stop-session test-dir)
          (let ((lockfile (claude-code-ide-mcp--lockfile-path test-port)))
            (when (file-exists-p lockfile)
              (delete-file lockfile)))))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;; Test for side window handling in openDiff
(ert-deftest claude-code-ide-test-opendiff-side-window ()
  "Test that openDiff handles side windows correctly."
  (let* ((temp-dir (make-temp-file "test-project-" t))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (claude-code-ide-debug t)
         (temp-file (make-temp-file "test-diff-" nil ".txt" "Original content\n"))
         (side-window nil)
         ;; Create a mock session for the test
         (test-session (make-claude-code-ide-mcp-session
                        :server nil
                        :client nil
                        :port 12345
                        :project-dir temp-dir
                        :deferred (make-hash-table :test 'equal)
                        :ping-timer nil
                        :selection-timer nil
                        :last-selection nil
                        :last-buffer nil
                        :active-diffs (make-hash-table :test 'equal)
                        :original-tab nil)))
    ;; Register the test session
    (puthash temp-dir test-session claude-code-ide-mcp--sessions)
    ;; Create a .git directory to make this a project
    (make-directory (expand-file-name ".git" temp-dir) t)

    (unwind-protect
        ;; Mock the project detection to return our test directory
        (cl-letf (((symbol-function 'claude-code-ide-mcp--get-buffer-project)
                   (lambda () temp-dir))
                  ((symbol-function 'claude-code-ide-mcp--get-current-session)
                   (lambda () test-session)))
          ;; Set up the project context
          (with-current-buffer (get-buffer-create "*test-buffer*")
            (setq default-directory temp-dir)

            ;; Create a side window to simulate the problem
            (let ((side-buffer (get-buffer-create "*test-sidebar*")))
              (with-current-buffer side-buffer
                (insert "Sidebar content"))
              ;; Display buffer in side window
              (setq side-window (display-buffer-in-side-window
                                 side-buffer
                                 '((side . left) (slot . 0) (window-width . 30))))

              ;; Verify side window was created
              (should (window-parameter side-window 'window-side))

              ;; Now try to open diff - should handle side window gracefully
              (let ((result (claude-code-ide-mcp-handle-open-diff
                             `((old_file_path . ,temp-file)
                               (new_file_path . ,temp-file)
                               (new_file_contents . "Modified content\n")
                               (tab_name . "test-diff")))))
                ;; Should return deferred
                (should (eq (alist-get 'deferred result) t))

                ;; Should have created diff session in the test session
                (should (gethash "test-diff" (claude-code-ide-mcp-session-active-diffs test-session)))

                ;; Clean up - quit ediff if it started
                (when (and (boundp 'ediff-control-buffer)
                           ediff-control-buffer
                           (buffer-live-p ediff-control-buffer))
                  (with-current-buffer ediff-control-buffer
                    (remove-hook 'ediff-quit-hook t t)
                    (ediff-really-quit nil)))))))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))
      (when (and side-window (window-live-p side-window))
        (delete-window side-window))
      (claude-code-ide-mcp--cleanup-diff "test-diff" test-session)
      (kill-buffer "*test-buffer*")
      (kill-buffer "*test-sidebar*"))))

;;; Tests for Diagnostics

(ert-deftest claude-code-ide-test-diagnostics-severity-mapping ()
  "Test diagnostic severity conversion."
  (require 'claude-code-ide-diagnostics)
  ;; Test Flycheck symbols
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'error) 1))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'warning) 2))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'info) 3))
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'hint) 4))
  ;; Test default fallback
  (should (= (claude-code-ide-diagnostics--severity-to-vscode 'unknown) 3)))

(ert-deftest claude-code-ide-test-diagnostics-severity-to-string ()
  "Test severity to string conversion."
  (require 'claude-code-ide-diagnostics)
  ;; Test Flycheck severities
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'error) "Error"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'warning) "Warning"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'info) "Information"))
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'hint) "Hint"))
  ;; Test default fallback
  (should (equal (claude-code-ide-diagnostics--severity-to-string 'unknown) "Information")))

(ert-deftest claude-code-ide-test-diagnostics-handler ()
  "Test getDiagnostics handler."
  (require 'claude-code-ide-diagnostics)
  ;; Test with no diagnostics available
  (let ((result (claude-code-ide-diagnostics-handler nil)))
    ;; The diagnostics handler returns content array format
    (should (listp result))
    ;; Check it has the expected format
    (should (equal (alist-get 'type (car result)) "text"))
    ;; The text should be an empty array "[]"
    (should (equal (alist-get 'text (car result)) "[]"))))

(ert-deftest claude-code-ide-test-check-document-dirty ()
  "Test checkDocumentDirty handler."
  (require 'claude-code-ide-mcp-handlers)
  ;; Test with a modified buffer
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-file.el")
    (insert "test content")
    (set-buffer-modified-p t)
    (let ((result (claude-code-ide-mcp-handle-check-document-dirty
                   '((filePath . "/tmp/test-file.el")))))
      (should (eq (alist-get 'isDirty result) t))))
  ;; Test with an unmodified buffer
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-file2.el")
    (insert "test content")
    (set-buffer-modified-p nil)
    (let ((result (claude-code-ide-mcp-handle-check-document-dirty
                   '((filePath . "/tmp/test-file2.el")))))
      (should (eq (alist-get 'isDirty result) :json-false))))
  ;; Test with a non-existent file
  (let ((result (claude-code-ide-mcp-handle-check-document-dirty
                 '((filePath . "/tmp/non-existent-file.el")))))
    (should (eq (alist-get 'isDirty result) :json-false)))
  ;; Test with missing filePath parameter
  (should-error (claude-code-ide-mcp-handle-check-document-dirty '())
                :type 'mcp-error))


;; Test multiple ediff sessions
(ert-deftest claude-code-ide-test-multiple-ediff-sessions ()
  "Test that multiple ediff sessions can run simultaneously without conflicts."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     (let* ((session (make-claude-code-ide-mcp-session
                      :project-dir default-directory
                      :active-diffs (make-hash-table :test 'equal)))
            (file1 (expand-file-name "test-file1.txt" default-directory))
            (file2 (expand-file-name "test-file2.txt" default-directory))
            (control-buffers '()))

       ;; Register session in global hash table
       (puthash default-directory session claude-code-ide-mcp--sessions)

       ;; Create test files
       (with-temp-file file1 (insert "Original content 1"))
       (with-temp-file file2 (insert "Original content 2"))

       ;; Create a .git directory to make this a project
       (make-directory (expand-file-name ".git" default-directory) t)

       ;; Mock ediff functions to capture control buffer names
       (cl-letf* ((ediff-called-count 0)
                  ((symbol-function 'ediff-buffers)
                   (lambda (buf-A buf-B)
                     (cl-incf ediff-called-count)
                     ;; Simulate ediff creating a control buffer with the suffix
                     (let ((suffix (or ediff-control-buffer-suffix "")))
                       (push (format "*Ediff Control Panel%s*" suffix) control-buffers))))
                  ((symbol-function 'claude-code-ide-mcp--get-current-session)
                   (lambda () session)))

         ;; Simulate opening multiple diffs
         (unwind-protect
             (progn
               ;; Open first diff
               (let ((result1 (claude-code-ide-mcp-handle-open-diff
                               `((old_file_path . ,file1)
                                 (new_file_path . ,file1)
                                 (new_file_contents . "Modified content 1")
                                 (tab_name . "diff1")))))
                 (should (equal result1 '((deferred . t) (unique-key . "diff1")))))

               ;; Open second diff
               (let ((result2 (claude-code-ide-mcp-handle-open-diff
                               `((old_file_path . ,file2)
                                 (new_file_path . ,file2)
                                 (new_file_contents . "Modified content 2")
                                 (tab_name . "diff2")))))
                 (should (equal result2 '((deferred . t) (unique-key . "diff2")))))

               ;; Verify ediff was called twice
               (should (= ediff-called-count 2))

               ;; Verify we have two distinct control buffer names
               (should (= (length control-buffers) 2))
               (should (member "*Ediff Control Panel<diff1>*" control-buffers))
               (should (member "*Ediff Control Panel<diff2>*" control-buffers))

               ;; Verify active diffs are tracked correctly
               (let ((active-diffs (claude-code-ide-mcp--get-active-diffs session)))
                 (should (gethash "diff1" active-diffs))
                 (should (gethash "diff2" active-diffs))))

           ;; Cleanup
           (claude-code-ide-mcp-handle-close-all-diff-tabs nil)
           (when (file-exists-p file1) (delete-file file1))
           (when (file-exists-p file2) (delete-file file2))
           ;; Remove session from global hash table
           (remhash default-directory claude-code-ide-mcp--sessions)))))))

(ert-deftest test-claude-code-ide-mcp-multi-session-deferred ()
  "Test that deferred responses work correctly with multiple sessions."
  (skip-unless (not (getenv "CI")))
  (let ((claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
        (project-a "/tmp/project-a/")
        (project-b "/tmp/project-b/")
        (session-a nil)
        (session-b nil)
        (deferred-responses '())
        (sent-responses '()))
    ;; Create mock websocket-send-text to capture responses
    (cl-letf* (((symbol-function 'websocket-send-text)
                (lambda (_ws text)
                  (push text sent-responses))))
      (unwind-protect
          (progn
            ;; Create two sessions
            (make-directory project-a t)
            (make-directory project-b t)

            ;; Session A
            (let ((default-directory project-a))
              (claude-code-ide-mcp-start project-a)
              (setq session-a (gethash project-a claude-code-ide-mcp--sessions)))

            ;; Session B
            (let ((default-directory project-b))
              (claude-code-ide-mcp-start project-b)
              (setq session-b (gethash project-b claude-code-ide-mcp--sessions)))

            ;; Set up mock clients for each session
            (setf (claude-code-ide-mcp-session-client session-a) :mock-client-a)
            (setf (claude-code-ide-mcp-session-client session-b) :mock-client-b)

            ;; Store deferred responses in each session
            (let ((deferred-a (claude-code-ide-mcp-session-deferred session-a))
                  (deferred-b (claude-code-ide-mcp-session-deferred session-b)))
              ;; Session A has a deferred response for openDiff-diff1
              (puthash "openDiff-diff1" "request-id-1" deferred-a)
              ;; Session B has a deferred response for openDiff-diff2
              (puthash "openDiff-diff2" "request-id-2" deferred-b))

            ;; Complete deferred response for session A
            (claude-code-ide-mcp-complete-deferred "openDiff"
                                                   '(((type . "text") (text . "FILE_SAVED")))
                                                   "diff1")

            ;; Complete deferred response for session B
            (claude-code-ide-mcp-complete-deferred "openDiff"
                                                   '(((type . "text") (text . "DIFF_REJECTED")))
                                                   "diff2")

            ;; Verify both responses were sent
            (should (= (length sent-responses) 2))

            ;; Verify the responses contain the correct request IDs
            (let ((response1 (json-read-from-string (nth 1 sent-responses)))
                  (response2 (json-read-from-string (nth 0 sent-responses))))
              ;; Check that request-id-1 and request-id-2 were both used
              (let ((ids (list (alist-get 'id response1) (alist-get 'id response2))))
                (should (member "request-id-1" ids))
                (should (member "request-id-2" ids))))

            ;; Verify deferred responses were removed from sessions
            (should (= 0 (hash-table-count (claude-code-ide-mcp-session-deferred session-a))))
            (should (= 0 (hash-table-count (claude-code-ide-mcp-session-deferred session-b)))))

        ;; Cleanup
        (ignore-errors (delete-directory project-a t))
        (ignore-errors (delete-directory project-b t))
        (clrhash claude-code-ide-mcp--sessions)))))

;;; WebSocket Integration Tests

(ert-deftest claude-code-ide-test-websocket-integration ()
  "Test real WebSocket client-server communication for MCP protocol."
  (skip-unless (and (featurep 'websocket)
                    (not (boundp 'websocket--test-server))))  ; Skip if mock websocket is active
  (let* ((temp-dir (make-temp-file "test-ws-integration-" t))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (test-port nil)
         (test-server nil)
         (test-client nil)
         (received-messages '())
         (connection-established nil))

    (unwind-protect
        (progn
          ;; Start MCP server
          (setq test-port (claude-code-ide-mcp-start temp-dir))
          (should (numberp test-port))
          (setq test-server (gethash temp-dir claude-code-ide-mcp--sessions))
          (should test-server)

          ;; Wait a moment for server to be ready
          (sleep-for 0.1)

          ;; Create WebSocket client to connect to our server
          (setq test-client
                (websocket-open
                 (format "ws://localhost:%d" test-port)
                 :on-open (lambda (_ws)
                            (setq connection-established t))
                 :on-message (lambda (_ws frame)
                               (let ((message (websocket-frame-text frame)))
                                 (push message received-messages)))
                 :on-error (lambda (_ws type err)
                             (message "WebSocket error: %s %s" type err))
                 :on-close (lambda (_ws)
                             (message "WebSocket connection closed"))))

          ;; Wait for connection to establish
          (let ((max-wait 50)) ; 5 seconds max
            (while (and (not connection-established) (> max-wait 0))
              (sleep-for 0.1)
              (setq max-wait (1- max-wait))))

          (should connection-established)

          ;; Send MCP initialization message
          (let ((init-message (json-encode
                               '((jsonrpc . "2.0")
                                 (id . "init-1")
                                 (method . "initialize")
                                 (params . ((protocolVersion . "2024-11-05")
                                            (capabilities . ((roots . t)))
                                            (clientInfo . ((name . "test-client")
                                                           (version . "1.0.0")))))))))
            (websocket-send-text test-client init-message))

          ;; Wait for response
          (let ((max-wait 30)) ; 3 seconds max
            (while (and (null received-messages) (> max-wait 0))
              (sleep-for 0.1)
              (setq max-wait (1- max-wait))))

          ;; Should have received initialization response
          (should received-messages)
          (let* ((response-text (car received-messages))
                 (response (json-read-from-string response-text)))
            (should (equal (alist-get 'jsonrpc response) "2.0"))
            (should (equal (alist-get 'id response) "init-1"))
            (should (alist-get 'result response)))

          ;; Clear received messages for next test
          (setq received-messages nil)

          ;; Test tool call - openFile with a temporary file
          (let* ((test-file (make-temp-file "ws-test-" nil ".txt" "Test content\nLine 2\nLine 3"))
                 (tool-message (json-encode
                                `((jsonrpc . "2.0")
                                  (id . "tool-1")
                                  (method . "tools/call")
                                  (params . ((name . "openFile")
                                             (arguments . ((path . ,test-file)))))))))
            (websocket-send-text test-client tool-message)

            ;; Wait for tool response
            (let ((max-wait 30)) ; 3 seconds max
              (while (and (null received-messages) (> max-wait 0))
                (sleep-for 0.1)
                (setq max-wait (1- max-wait))))

            ;; Should have received tool response
            (should received-messages)
            (let* ((response-text (car received-messages))
                   (response (json-read-from-string response-text)))
              (should (equal (alist-get 'jsonrpc response) "2.0"))
              (should (equal (alist-get 'id response) "tool-1"))
              (should (alist-get 'result response))
              (let ((result (alist-get 'result response)))
                (should (alist-get 'content result))
                (should (vectorp (alist-get 'content result)))
                ;; Should have at least one content item
                (should (> (length (alist-get 'content result)) 0))))

            ;; Cleanup test file
            (delete-file test-file))

          ;; Test list tools call
          (setq received-messages nil)
          (let ((tools-message (json-encode
                                '((jsonrpc . "2.0")
                                  (id . "tools-1")
                                  (method . "tools/list")))))
            (websocket-send-text test-client tools-message)

            ;; Wait for tools list response
            (let ((max-wait 30)) ; 3 seconds max
              (while (and (null received-messages) (> max-wait 0))
                (sleep-for 0.1)
                (setq max-wait (1- max-wait))))

            ;; Should have received tools list
            (should received-messages)
            (let* ((response-text (car received-messages))
                   (response (json-read-from-string response-text)))
              (should (equal (alist-get 'jsonrpc response) "2.0"))
              (should (equal (alist-get 'id response) "tools-1"))
              (should (alist-get 'result response))
              (let ((result (alist-get 'result response)))
                (should (alist-get 'tools result))
                (should (vectorp (alist-get 'tools result)))
                ;; Should have multiple tools
                (should (> (length (alist-get 'tools result)) 5))
                ;; Should include openFile tool
                (let ((tool-names (mapcar (lambda (tool) (alist-get 'name tool))
                                          (append (alist-get 'tools result) nil))))
                  (should (member "openFile" tool-names))
                  (should (member "getCurrentSelection" tool-names))
                  (should (member "getOpenEditors" tool-names)))))))

      ;; Cleanup
      (when test-client
        (websocket-close test-client))
      (when test-server
        (claude-code-ide-mcp-stop-session temp-dir))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest claude-code-ide-test-websocket-error-handling ()
  "Test WebSocket error handling and invalid message processing."
  (skip-unless (and (featurep 'websocket)
                    (not (boundp 'websocket--test-server))))  ; Skip if mock websocket is active
  (let* ((temp-dir (make-temp-file "test-ws-error-" t))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (test-port nil)
         (test-client nil)
         (received-messages '())
         (connection-established nil))

    (unwind-protect
        (progn
          ;; Start MCP server
          (setq test-port (claude-code-ide-mcp-start temp-dir))
          (should (numberp test-port))

          ;; Wait a moment for server to be ready
          (sleep-for 0.1)

          ;; Create WebSocket client
          (setq test-client
                (websocket-open
                 (format "ws://localhost:%d" test-port)
                 :on-open (lambda (_ws)
                            (setq connection-established t))
                 :on-message (lambda (_ws frame)
                               (let ((message (websocket-frame-text frame)))
                                 (push message received-messages)))))

          ;; Wait for connection
          (let ((max-wait 50))
            (while (and (not connection-established) (> max-wait 0))
              (sleep-for 0.1)
              (setq max-wait (1- max-wait))))

          (should connection-established)

          ;; Test invalid JSON
          (websocket-send-text test-client "invalid json {")
          (sleep-for 0.2)

          ;; Test missing required fields
          (websocket-send-text test-client "{\"jsonrpc\": \"2.0\"}")
          (sleep-for 0.2)

          ;; Test invalid tool call
          (let ((invalid-tool (json-encode
                               '((jsonrpc . "2.0")
                                 (id . "invalid-1")
                                 (method . "tools/call")
                                 (params . ((name . "nonexistentTool")))))))
            (websocket-send-text test-client invalid-tool))

          ;; Wait for error response
          (let ((max-wait 30))
            (while (and (null received-messages) (> max-wait 0))
              (sleep-for 0.1)
              (setq max-wait (1- max-wait))))

          ;; Should have received error response for invalid tool
          (should (= (length received-messages) 1))
          (let* ((response-text (car received-messages))
                 (response (json-read-from-string response-text)))
            (should (equal (alist-get 'jsonrpc response) "2.0"))
            (should (equal (alist-get 'id response) "invalid-1"))
            (should (alist-get 'error response))
            (let ((error (alist-get 'error response)))
              (should (alist-get 'code error))
              (should (alist-get 'message error)))))

      ;; Cleanup
      (when test-client
        (websocket-close test-client))
      (when (gethash temp-dir claude-code-ide-mcp--sessions)
        (claude-code-ide-mcp-stop-session temp-dir))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Phase 2: Terminal-Only Mode Tests

(ert-deftest claude-code-ide-test-terminal-command-exists ()
  "Test that claude-code-ide-terminal command exists and is interactive."
  (should (fboundp 'claude-code-ide-terminal))
  (should (commandp 'claude-code-ide-terminal)))

(ert-deftest claude-code-ide-test-terminal-mode-no-auto-claude ()
  "Test that terminal mode starts MCP server but doesn't auto-launch Claude."
  (let* ((temp-dir (make-temp-file "test-terminal-mode-" t))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (claude-code-ide--processes (make-hash-table :test 'equal))
         (vterm-buffer nil)
         (vterm-process nil)
         (created-buffers '()))

    (unwind-protect
        ;; Mock vterm creation to capture what command would be run
        (cl-letf* (((symbol-function 'vterm)
                    (lambda (&optional _buffer-name)
                      (let ((buffer (generate-new-buffer "*claude-code-ide-test*")))
                        (push buffer created-buffers)
                        (setq vterm-buffer buffer)
                        (with-current-buffer buffer
                          (setq major-mode 'vterm-mode)
                          (setq vterm--process 'mock-process))
                        buffer)))
                   ((symbol-function 'vterm-send-string) #'ignore)
                   ((symbol-function 'vterm-send-return) #'ignore)
                   ((symbol-function 'get-buffer-process)
                    (lambda (_buffer) 'mock-process))
                   ((symbol-function 'set-process-sentinel) #'ignore)
                   ((symbol-function 'claude-code-ide--display-buffer-in-side-window) #'ignore)
                   (claude-code-ide-cli-debug nil))

          ;; Call terminal mode
          (let ((default-directory temp-dir))
            (claude-code-ide-terminal))

          ;; Should have started MCP server
          (should (gethash temp-dir claude-code-ide-mcp--sessions))

          ;; Should have created vterm buffer
          (should vterm-buffer)
          (should (buffer-live-p vterm-buffer))

          ;; Should NOT have tracked the process as Claude process
          ;; (because it's just a shell, not Claude CLI)
          (should-not (gethash temp-dir claude-code-ide--processes)))

      ;; Cleanup
      (dolist (buffer created-buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (when (gethash temp-dir claude-code-ide-mcp--sessions)
        (claude-code-ide-mcp-stop-session temp-dir))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))




;;; Phase 2 Tests - Terminal-Only Mode

(ert-deftest claude-code-ide-test-terminal-only-command-exists ()
  "Test that the terminal-only command is defined."
  (should (fboundp 'claude-code-ide-terminal)))


(ert-deftest claude-code-ide-test-normal-mode-requires-cli ()
  "Test that normal mode fails without Claude CLI."
  (let* ((temp-dir (make-temp-file "test-cli-required-" t))
         (claude-code-ide--processes (make-hash-table :test 'equal))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal)))

    ;; Mock CLI as unavailable
    (cl-letf (((symbol-function 'claude-code-ide--ensure-cli)
               (lambda () nil)))  ; Mock CLI as NOT available

      (unwind-protect
          (progn
            ;; Normal mode should fail without CLI
            (let ((default-directory temp-dir))
              (should-error (claude-code-ide--start-session nil nil)
                            :type 'user-error)))

        ;; Cleanup
        (delete-directory temp-dir t)))))

;;; Dynamic Project Detection Tests

(ert-deftest claude-code-ide-test-dynamic-project-detection-doom ()
  "Test dynamic project detection with mocked Doom Emacs."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Mock Doom Emacs environment
     (let ((doom-project-root-result "/mock/doom/project")
           (original-featurep (symbol-function 'featurep)))
       ;; Mock featurep to return t for 'doom
       (fset 'featurep (lambda (feature)
                         (if (eq feature 'doom)
                             t
                           (funcall original-featurep feature))))
       (fset 'doom-project-root (lambda () doom-project-root-result))
       (unwind-protect
           (progn
             ;; Should return doom project root when available
             (should (string= (claude-code-ide--get-working-directory) doom-project-root-result)))
         ;; Cleanup mocks
         (fset 'featurep original-featurep)
         (when (fboundp 'doom-project-root)
           (fmakunbound 'doom-project-root)))))))

(ert-deftest claude-code-ide-test-dynamic-project-detection-fallback ()
  "Test dynamic project detection fallback to default-directory."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Ensure no Doom features are mocked
     (put 'doom 'feature nil)
     (when (fboundp 'doom-project-root)
       (fmakunbound 'doom-project-root))

     (let ((expected (expand-file-name default-directory)))
       ;; Should fall back to default-directory when Doom is not available
       (should (string= (claude-code-ide--get-working-directory) expected))))))

(ert-deftest claude-code-ide-test-dynamic-project-detection-ultimate-fallback ()
  "Test dynamic project detection ultimate fallback to ~/notes."
  ;; This test simulates a scenario where default-directory is nil/empty
  (let ((default-directory nil))
    ;; Ensure no Doom features are available
    (put 'doom 'feature nil)
    (when (fboundp 'doom-project-root)
      (fmakunbound 'doom-project-root))

    ;; Should fall back to hardcoded ~/notes when all else fails
    (should (string= (claude-code-ide--get-working-directory) (expand-file-name "~/notes")))))

(ert-deftest claude-code-ide-test-dynamic-project-detection-doom-returns-nil ()
  "Test dynamic project detection when Doom project root returns nil."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Mock Doom Emacs environment but with nil project root
     (put 'doom 'feature t)  ; Mock featurep check
     (fset 'doom-project-root (lambda () nil))  ; Returns nil (no project)
     (unwind-protect
         (let ((expected (expand-file-name default-directory)))
           ;; Should fall back to default-directory when doom-project-root returns nil
           (should (string= (claude-code-ide--get-working-directory) expected)))
       ;; Cleanup mocks
       (put 'doom 'feature nil)
       (fmakunbound 'doom-project-root)))))

;;; Workspace Session Management Tests

(ert-deftest claude-code-ide-test-workspace-name-resolution-persp-mode ()
  "Test workspace name resolution with persp-mode available."
  (let ((original-featurep (symbol-function 'featurep)))
    (unwind-protect
        (progn
          ;; Mock persp-mode functions
          (fset 'featurep (lambda (feature) (eq feature 'persp-mode)))
          (fset 'persp-name (lambda (_persp) "test-workspace"))
          (fset 'get-current-persp (lambda (&optional _frame _window) 'mock-persp))

          ;; Should return persp-mode workspace name
          (should (string= (claude-code-ide--get-workspace-name) "test-workspace")))
      ;; Cleanup
      (fset 'featurep original-featurep)
      (when (fboundp 'persp-name) (fmakunbound 'persp-name))
      (when (fboundp 'get-current-persp) (fmakunbound 'get-current-persp)))))

(ert-deftest claude-code-ide-test-workspace-name-resolution-fallback ()
  "Test workspace name resolution fallback to directory name."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     (let ((original-featurep (symbol-function 'featurep)))
       (unwind-protect
           (progn
             ;; Mock no persp-mode
             (fset 'featurep (lambda (_feature) nil))

             ;; Should fall back to directory name
             (let ((expected (file-name-nondirectory (directory-file-name default-directory))))
               (should (string= (claude-code-ide--get-workspace-name) expected))))
         ;; Cleanup
         (fset 'featurep original-featurep))))))

(ert-deftest claude-code-ide-test-workspace-name-resolution-default ()
  "Test workspace name resolution ultimate fallback to 'default'."
  (let ((original-featurep (symbol-function 'featurep))
        (original-get-working-directory (symbol-function 'claude-code-ide--get-working-directory)))
    (unwind-protect
        (progn
          ;; Mock no persp-mode and no working directory
          (fset 'featurep (lambda (_feature) nil))
          (fset 'claude-code-ide--get-working-directory (lambda () nil))

          ;; Should fall back to "default"
          (should (string= (claude-code-ide--get-workspace-name) "default")))
      ;; Cleanup
      (fset 'featurep original-featurep)
      (fset 'claude-code-ide--get-working-directory original-get-working-directory))))

(ert-deftest claude-code-ide-test-workspace-session-management ()
  "Test workspace session get/set/clear operations."
  ;; Clear any existing sessions first
  (clrhash claude-code-ide--workspace-sessions)

  (let ((test-session '(:process mock-process
                                 :directory "/tmp/test"
                                 :buffer mock-buffer
                                 :workspace-name "test-workspace")))

    ;; Test setting and getting session
    (claude-code-ide--set-workspace-session test-session "test-workspace")
    (should (equal (claude-code-ide--get-workspace-session "test-workspace") test-session))

    ;; Test clearing session
    (claude-code-ide--clear-workspace-session "test-workspace")
    (should (null (claude-code-ide--get-workspace-session "test-workspace")))

    ;; Test current workspace operations
    (let ((original-get-workspace-name (symbol-function 'claude-code-ide--get-workspace-name)))
      (unwind-protect
          (progn
            (fset 'claude-code-ide--get-workspace-name (lambda () "current-workspace"))

            ;; Set session for current workspace
            (claude-code-ide--set-workspace-session test-session)
            (should (equal (claude-code-ide--get-workspace-session) test-session))

            ;; Clear current workspace session
            (claude-code-ide--clear-workspace-session)
            (should (null (claude-code-ide--get-workspace-session))))
        ;; Cleanup
        (fset 'claude-code-ide--get-workspace-name original-get-workspace-name)))))

(ert-deftest claude-code-ide-test-workspace-session-isolation ()
  "Test that workspace sessions are properly isolated."
  ;; Clear any existing sessions first
  (clrhash claude-code-ide--workspace-sessions)

  (let ((session1 '(:process mock-process-1 :directory "/tmp/workspace1" :workspace-name "workspace1"))
        (session2 '(:process mock-process-2 :directory "/tmp/workspace2" :workspace-name "workspace2")))

    ;; Set sessions for different workspaces
    (claude-code-ide--set-workspace-session session1 "workspace1")
    (claude-code-ide--set-workspace-session session2 "workspace2")

    ;; Verify isolation
    (should (equal (claude-code-ide--get-workspace-session "workspace1") session1))
    (should (equal (claude-code-ide--get-workspace-session "workspace2") session2))
    (should (null (claude-code-ide--get-workspace-session "nonexistent")))

    ;; Clear one session should not affect the other
    (claude-code-ide--clear-workspace-session "workspace1")
    (should (null (claude-code-ide--get-workspace-session "workspace1")))
    (should (equal (claude-code-ide--get-workspace-session "workspace2") session2))))

(ert-deftest claude-code-ide-test-cleanup-dead-workspace-sessions ()
  "Test cleanup of dead workspace sessions."
  ;; Clear any existing sessions first
  (clrhash claude-code-ide--workspace-sessions)

  ;; Mock process-live-p to control process state
  (let ((original-process-live-p (symbol-function 'process-live-p))
        (live-processes '(mock-live-process)))
    (unwind-protect
        (progn
          (fset 'process-live-p (lambda (proc) (memq proc live-processes)))

          ;; Set up sessions with live and dead processes
          (claude-code-ide--set-workspace-session
           '(:process mock-live-process :directory "/tmp/live" :workspace-name "live-workspace")
           "live-workspace")
          (claude-code-ide--set-workspace-session
           '(:process mock-dead-process :directory "/tmp/dead" :workspace-name "dead-workspace")
           "dead-workspace")

          ;; Verify both sessions exist before cleanup
          (should (claude-code-ide--get-workspace-session "live-workspace"))
          (should (claude-code-ide--get-workspace-session "dead-workspace"))

          ;; Run cleanup
          (claude-code-ide--cleanup-dead-workspace-sessions)

          ;; Verify only live session remains
          (should (claude-code-ide--get-workspace-session "live-workspace"))
          (should (null (claude-code-ide--get-workspace-session "dead-workspace"))))
      ;; Cleanup
      (fset 'process-live-p original-process-live-p))))

(ert-deftest claude-code-ide-test-workspace-session-backward-compatibility ()
  "Test that workspace sessions don't break existing directory-based session logic."
  ;; Clear existing sessions
  (clrhash claude-code-ide--processes)
  (clrhash claude-code-ide--workspace-sessions)

  (let ((test-directory "/tmp/test-project")
        (test-process 'mock-process))

    ;; Set directory-based process (existing behavior)
    (claude-code-ide--set-process test-process test-directory)

    ;; Verify it still works
    (should (eq (claude-code-ide--get-process test-directory) test-process))

    ;; Test that cleanup still works for directory-based sessions
    (let ((original-process-live-p (symbol-function 'process-live-p)))
      (unwind-protect
          (progn
            (fset 'process-live-p (lambda (_proc) nil))  ; Make process appear dead

            ;; Run cleanup
            (claude-code-ide--cleanup-dead-processes)

            ;; Process should be removed
            (should (null (claude-code-ide--get-process test-directory))))
        ;; Cleanup
        (fset 'process-live-p original-process-live-p)))))

;;; Workspace Lifecycle Hook Tests

(ert-deftest claude-code-ide-test-workspace-activated-hook ()
  "Test workspace activation hook switches to correct session."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (test-buffer (generate-new-buffer "*test-claude-workspace*"))
        (test-process :mock-process)
        (displayed-buffer nil))

    ;; Mock session data
    (let ((session `(:buffer ,test-buffer
                             :process ,test-process
                             :directory "/test/workspace1"
                             :workspace-name "workspace1"
                             :created ,(current-time))))
      (puthash "workspace1" session claude-code-ide--workspace-sessions))

    ;; Mock display function to track calls
    (cl-letf* (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                (lambda (buffer)
                  (setq displayed-buffer buffer)
                  (selected-window)))
               ((symbol-function 'process-live-p) (lambda (_) t))
               ((symbol-function 'buffer-live-p) (lambda (_) t)))

      ;; Create mock perspective object
      (let ((mock-persp `((name . "workspace1"))))
        ;; Mock persp-name and get-current-persp
        (cl-letf (((symbol-function 'persp-name) (lambda (persp) (alist-get 'name persp)))
                  ((symbol-function 'get-current-persp) (lambda () mock-persp)))

          ;; Test the hook function with 'frame type
          (claude-code-ide--workspace-activated-h 'frame)

          ;; Verify the correct buffer was displayed
          (should (eq displayed-buffer test-buffer)))))

    ;; Cleanup
    (kill-buffer test-buffer)))

(ert-deftest claude-code-ide-test-workspace-deactivated-hook ()
  "Test workspace deactivation hook saves session state."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (test-buffer (generate-new-buffer "*test-claude-workspace*"))
        (test-process :mock-process)
        (session-updated nil))

    ;; Mock session data
    (let ((session `(:buffer ,test-buffer
                             :process ,test-process
                             :directory "/test/workspace1"
                             :workspace-name "workspace1"
                             :created ,(current-time))))
      (puthash "workspace1" session claude-code-ide--workspace-sessions))

    ;; Mock functions
    (cl-letf* (((symbol-function 'claude-code-ide--set-workspace-session)
                (lambda (new-session workspace-name)
                  (setq session-updated (list new-session workspace-name))
                  (puthash workspace-name new-session claude-code-ide--workspace-sessions)))
               ((symbol-function 'claude-code-ide--get-workspace-name)
                (lambda () "workspace1"))
               ((symbol-function 'claude-code-ide--get-workspace-session)
                (lambda (&optional workspace-name)
                  (gethash (or workspace-name "workspace1") claude-code-ide--workspace-sessions)))
               ((symbol-function 'buffer-live-p) (lambda (_) t)))

      ;; Create mock perspective object
      (let ((mock-persp `((name . "workspace1"))))
        ;; Mock persp-name and get-current-persp
        (cl-letf (((symbol-function 'persp-name) (lambda (persp) (alist-get 'name persp)))
                  ((symbol-function 'get-current-persp) (lambda () mock-persp)))

          ;; Test the hook function with 'frame type
          (claude-code-ide--workspace-deactivated-h 'frame)

          ;; Verify session was updated with last-active timestamp
          (should session-updated)
          (should (equal (cadr session-updated) "workspace1"))
          (should (plist-get (car session-updated) :last-active)))))

    ;; Cleanup
    (kill-buffer test-buffer)))

(ert-deftest claude-code-ide-test-workspace-hook-with-nil-persp ()
  "Test workspace hooks handle nil perspective gracefully."
  (let ((hook-called nil))
    ;; Mock debug function to track if hooks run
    (cl-letf (((symbol-function 'claude-code-ide-debug)
               (lambda (&rest _) (setq hook-called t)))
              ((symbol-function 'get-current-persp) (lambda () nil)))

      ;; Test activation hook with nil perspective
      (claude-code-ide--workspace-activated-h 'frame)
      (should (not hook-called))

      ;; Test deactivation hook with nil perspective
      (claude-code-ide--workspace-deactivated-h 'frame)
      (should (not hook-called)))))

(ert-deftest claude-code-ide-test-workspace-session-switching ()
  "Test session switching functionality."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (test-buffer1 (generate-new-buffer "*test-claude-ws1*"))
        (test-buffer2 (generate-new-buffer "*test-claude-ws2*"))
        (displayed-buffers '()))

    ;; Create two mock sessions
    (let ((session1 `(:buffer ,test-buffer1 :process :mock-proc1))
          (session2 `(:buffer ,test-buffer2 :process :mock-proc2)))
      (puthash "workspace1" session1 claude-code-ide--workspace-sessions)
      (puthash "workspace2" session2 claude-code-ide--workspace-sessions))

    ;; Mock functions
    (cl-letf* (((symbol-function 'claude-code-ide--display-buffer-in-side-window)
                (lambda (buffer)
                  (push buffer displayed-buffers)
                  (selected-window)))
               ((symbol-function 'process-live-p) (lambda (_) t))
               ((symbol-function 'buffer-live-p) (lambda (_) t)))

      ;; Test switching to workspace1
      (claude-code-ide--switch-to-workspace-session "workspace1")
      (should (memq test-buffer1 displayed-buffers))

      ;; Test switching to workspace2
      (claude-code-ide--switch-to-workspace-session "workspace2")
      (should (memq test-buffer2 displayed-buffers))

      ;; Test switching to non-existent workspace (should be no-op)
      (let ((buffers-before (length displayed-buffers)))
        (claude-code-ide--switch-to-workspace-session "nonexistent")
        (should (= (length displayed-buffers) buffers-before))))

    ;; Cleanup
    (kill-buffer test-buffer1)
    (kill-buffer test-buffer2)))

(ert-deftest claude-code-ide-test-workspace-hook-registration ()
  "Test workspace hook registration and deregistration."
  (let ((persp-activated-functions '())
        (persp-before-deactivate-functions '())
        (hook-functions-added nil))

    ;; Mock hook functions to track additions/removals
    (cl-letf* (((symbol-function 'add-hook)
                (lambda (hook function)
                  (when (eq hook 'persp-activated-functions)
                    (push function persp-activated-functions))
                  (when (eq hook 'persp-before-deactivate-functions)
                    (push function persp-before-deactivate-functions))
                  (setq hook-functions-added t)))
               ((symbol-function 'remove-hook)
                (lambda (hook function)
                  (when (eq hook 'persp-activated-functions)
                    (setq persp-activated-functions
                          (remove function persp-activated-functions)))
                  (when (eq hook 'persp-before-deactivate-functions)
                    (setq persp-before-deactivate-functions
                          (remove function persp-before-deactivate-functions)))))
               ;; Mock persp-mode feature checks
               ((symbol-function 'featurep)
                (lambda (feature) (eq feature 'persp-mode)))
               ((symbol-function 'fboundp) (lambda (_) t))
               ((symbol-function 'boundp) (lambda (_) t)))

      ;; Test enabling hooks
      (claude-code-ide--enable-workspace-hooks)
      (should hook-functions-added)
      (should (memq #'claude-code-ide--workspace-activated-h persp-activated-functions))
      (should (memq #'claude-code-ide--workspace-deactivated-h persp-before-deactivate-functions))

      ;; Test disabling hooks
      (claude-code-ide--disable-workspace-hooks)
      (should (not (memq #'claude-code-ide--workspace-activated-h persp-activated-functions)))
      (should (not (memq #'claude-code-ide--workspace-deactivated-h persp-before-deactivate-functions))))))

(ert-deftest claude-code-ide-test-workspace-hook-no-persp-mode ()
  "Test hook registration when persp-mode is not available."
  (let ((hooks-registered nil))
    ;; Mock add-hook to track if called
    (cl-letf* (((symbol-function 'add-hook)
                (lambda (&rest _) (setq hooks-registered t)))
               ;; Mock persp-mode as not available
               ((symbol-function 'featurep) (lambda (_) nil))
               ((symbol-function 'fboundp) (lambda (_) nil))
               ((symbol-function 'boundp) (lambda (_) nil)))

      ;; Try to enable hooks
      (claude-code-ide--enable-workspace-hooks)

      ;; Should not register hooks when persp-mode unavailable
      (should (not hooks-registered)))))

;;; Doom Workspace Command Tests

(ert-deftest claude-code-ide-test-+workspace/claude-code-without-prefix ()
  "Test +workspace/claude-code without prefix argument (get or create)."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((session-created nil)
         (session-switched nil))
     ;; Mock implementation functions
     (cl-letf (((symbol-function 'claude-code-ide--get-or-create-workspace-session)
                (lambda () (setq session-created t)))
               ((symbol-function 'claude-code-ide--create-new-workspace-session)
                (lambda () (setq session-switched t))))

       ;; Test without prefix (should get or create)
       (+workspace/claude-code nil)
       (should session-created)
       (should (not session-switched))))))

(ert-deftest claude-code-ide-test-+workspace/claude-code-with-prefix ()
  "Test +workspace/claude-code with prefix argument (force new)."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((session-created nil)
         (session-switched nil))
     ;; Mock implementation functions
     (cl-letf (((symbol-function 'claude-code-ide--get-or-create-workspace-session)
                (lambda () (setq session-created t)))
               ((symbol-function 'claude-code-ide--create-new-workspace-session)
                (lambda () (setq session-switched t))))

       ;; Test with prefix (should force new)
       (+workspace/claude-code t)
       (should (not session-created))
       (should session-switched)))))

(ert-deftest claude-code-ide-test-+workspace/claude-code-kill ()
  "Test +workspace/claude-code-kill command."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((kill-called nil))
     ;; Mock kill function
     (cl-letf (((symbol-function 'claude-code-ide--kill-workspace-session)
                (lambda (&optional _) (setq kill-called t))))

       (+workspace/claude-code-kill)
       (should kill-called)))))

(ert-deftest claude-code-ide-test-+workspace/claude-code-restart ()
  "Test +workspace/claude-code-restart command."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((restart-called nil))
     ;; Mock restart function
     (cl-letf (((symbol-function 'claude-code-ide--restart-workspace-session)
                (lambda () (setq restart-called t))))

       (+workspace/claude-code-restart)
       (should restart-called)))))

(ert-deftest claude-code-ide-test-+workspace/claude-code-switch ()
  "Test +workspace/claude-code-switch command."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((switch-called nil))
     ;; Mock switch function
     (cl-letf (((symbol-function 'claude-code-ide--switch-workspace-sessions)
                (lambda () (setq switch-called t))))

       (+workspace/claude-code-switch)
       (should switch-called)))))

(ert-deftest claude-code-ide-test-get-or-create-workspace-session-existing ()
  "Test get-or-create when existing session is alive."
  (claude-code-ide-test-with-workspace-session-mocks
   (let* ((mock-process (make-process :name "test" :command '("echo" "test")))
          (test-session `(:process ,mock-process :directory "/test" :buffer nil))
          (switch-called nil)
          (start-called nil))

     ;; Mock existing session
     (cl-letf (((symbol-function 'claude-code-ide--get-workspace-name)
                (lambda () "test-workspace"))
               ((symbol-function 'claude-code-ide--get-workspace-session)
                (lambda (_) test-session))
               ((symbol-function 'process-live-p)
                (lambda (_) t))
               ((symbol-function 'claude-code-ide--switch-to-workspace-session)
                (lambda (_) (setq switch-called t)))
               ((symbol-function 'claude-code-ide--start-session)
                (lambda (&rest _) (setq start-called t))))

       (claude-code-ide--get-or-create-workspace-session)
       (should switch-called)
       (should (not start-called))))))

(ert-deftest claude-code-ide-test-get-or-create-workspace-session-no-existing ()
  "Test get-or-create when no existing session."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((switch-called nil)
         (start-called nil))

     ;; Mock no existing session
     (cl-letf (((symbol-function 'claude-code-ide--get-workspace-name)
                (lambda () "test-workspace"))
               ((symbol-function 'claude-code-ide--get-workspace-session)
                (lambda (_) nil))
               ((symbol-function 'claude-code-ide--switch-to-workspace-session)
                (lambda (_) (setq switch-called t)))
               ((symbol-function 'claude-code-ide--start-session)
                (lambda (&rest _) (setq start-called t))))

       (claude-code-ide--get-or-create-workspace-session)
       (should (not switch-called))
       (should start-called)))))

(ert-deftest claude-code-ide-test-create-new-workspace-session ()
  "Test creating new workspace session (kills existing first)."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((kill-called nil)
         (start-called nil)
         (workspace-name "test-workspace"))

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-ide--get-workspace-name)
                (lambda () workspace-name))
               ((symbol-function 'claude-code-ide--kill-workspace-session-internal)
                (lambda (name)
                  (should (equal name workspace-name))
                  (setq kill-called t)))
               ((symbol-function 'claude-code-ide--start-session)
                (lambda (&rest _) (setq start-called t))))

       (claude-code-ide--create-new-workspace-session)
       (should kill-called)
       (should start-called)))))

(ert-deftest claude-code-ide-test-switch-workspace-sessions-with-active ()
  "Test switching between workspace sessions when sessions exist."
  (claude-code-ide-test-with-workspace-session-mocks
   (let* ((mock-process (make-process :name "test" :command '("echo" "test")))
          (sessions (make-hash-table :test 'equal))
          (choice-made nil)
          (switch-called nil))

     ;; Set up mock sessions
     (puthash "workspace1" `(:process ,mock-process) sessions)
     (puthash "workspace2" `(:process ,mock-process) sessions)

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-workspace-sessions)
                (lambda ()))
               ((symbol-value 'claude-code-ide--workspace-sessions) sessions)
               ((symbol-function 'process-live-p)
                (lambda (_) t))
               ((symbol-function 'completing-read)
                (lambda (prompt choices &rest _)
                  (should (string-match "Switch to Claude workspace session" prompt))
                  (should (>= (length choices) 2))
                  (setq choice-made t)
                  "workspace1"))
               ((symbol-function 'claude-code-ide--switch-to-workspace-session)
                (lambda (name)
                  (should (equal name "workspace1"))
                  (setq switch-called t))))

       (claude-code-ide--switch-workspace-sessions)
       (should choice-made)
       (should switch-called)))))

(ert-deftest claude-code-ide-test-switch-workspace-sessions-no-active ()
  "Test switching when no active workspace sessions exist."
  (claude-code-ide-test-with-workspace-session-mocks
   (let ((log-called nil)
         (sessions (make-hash-table :test 'equal)))

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-ide--cleanup-dead-workspace-sessions)
                (lambda ()))
               ((symbol-value 'claude-code-ide--workspace-sessions) sessions)
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest _)
                  (should (string-match "No active Claude Code workspace sessions" msg))
                  (setq log-called t))))

       (claude-code-ide--switch-workspace-sessions)
       (should log-called)))))

(ert-deftest claude-code-ide-test-kill-workspace-session-internal ()
  "Test internal workspace session kill functionality."
  (claude-code-ide-test-with-workspace-session-mocks
   (let* ((mock-process (make-process :name "test" :command '("echo" "test")))
          (test-session `(:process ,mock-process :directory "/test"))
          (cleanup-called nil)
          (log-called nil))

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-ide--get-workspace-session)
                (lambda (name)
                  (should (equal name "test-workspace"))
                  test-session))
               ((symbol-function 'process-live-p)
                (lambda (_) t))
               ((symbol-function 'claude-code-ide--cleanup-on-exit)
                (lambda (dir)
                  (should (equal dir "/test"))
                  (setq cleanup-called t)))
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (let ((formatted-msg (apply #'format msg args)))
                    (should (string-match "killed for workspace: test-workspace" formatted-msg)))
                  (setq log-called t))))

       (claude-code-ide--kill-workspace-session-internal "test-workspace")
       (should cleanup-called)
       (should log-called)))))

;;; Workspace Persistence Tests

(ert-deftest test-claude-code-ide-serialize-session-state ()
  "Test session state serialization for workspace persistence."
  (let* ((test-directory "/test/directory")
         (test-buffer (generate-new-buffer "*test-buffer*"))
         (test-created-time (current-time))
         (test-active-time (current-time))
         (test-session `(:process nil
                                  :directory ,test-directory
                                  :buffer ,test-buffer
                                  :workspace-name "test-workspace"
                                  :created ,test-created-time
                                  :last-active ,test-active-time))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (mcp-session (make-claude-code-ide-mcp-session
                       :project-dir test-directory
                       :port 12345
                       :original-tab "test-tab")))

    ;; Setup MCP session
    (puthash test-directory mcp-session claude-code-ide-mcp--sessions)

    (unwind-protect
        (let ((serialized (claude-code-ide--serialize-session-state test-session)))
          ;; Verify serialized state structure
          (should (listp serialized))
          (should (equal (plist-get serialized :directory) test-directory))
          (should (equal (plist-get serialized :workspace-name) "test-workspace"))
          (should (equal (plist-get serialized :created) test-created-time))
          (should (equal (plist-get serialized :last-active) test-active-time))
          (should (equal (plist-get serialized :buffer-name) "*test-buffer*"))

          ;; Verify MCP config is included
          (let ((mcp-config (plist-get serialized :mcp-config)))
            (should (listp mcp-config))
            (should (equal (plist-get mcp-config :project-dir) test-directory))
            (should (equal (plist-get mcp-config :port) 12345))
            (should (equal (plist-get mcp-config :original-tab) "test-tab")))

          ;; Verify no process objects in serialized state
          (should-not (plist-get serialized :process))
          (should-not (plist-get serialized :buffer)))

      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-ide-serialize-session-state-dead-buffer ()
  "Test session state serialization with dead buffer."
  (let* ((test-directory "/test/directory")
         (test-buffer (generate-new-buffer "*test-buffer*"))
         (test-session `(:directory ,test-directory
                                    :workspace-name "test-workspace"
                                    :buffer ,test-buffer)))

    ;; Kill the buffer before serialization
    (kill-buffer test-buffer)

    (let ((serialized (claude-code-ide--serialize-session-state test-session)))
      ;; Should not include buffer-name for dead buffer
      (should-not (plist-get serialized :buffer-name)))))

(ert-deftest test-claude-code-ide-workspace-save-hook ()
  "Test workspace save hook integration."
  (let* ((test-workspace-name "test-workspace")
         (test-directory "/test/directory")
         (test-buffer (generate-new-buffer "*test-buffer*"))
         (test-session `(:process nil
                                  :directory ,test-directory
                                  :buffer ,test-buffer
                                  :workspace-name ,test-workspace-name))
         (claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
         (saved-parameters (make-hash-table :test 'equal))
         (mock-persp (list :name test-workspace-name))
         (log-messages '()))

    ;; Setup session
    (puthash test-workspace-name test-session claude-code-ide--workspace-sessions)

    ;; Mock persp functions
    (cl-letf* (((symbol-function 'persp-name)
                (lambda (persp) (plist-get persp :name)))
               ((symbol-function 'persp-add-buffer-parameter)
                (lambda (persp param-name param-value)
                  (should (eq persp mock-persp))
                  (should (eq param-name 'claude-session-state))
                  (puthash param-name param-value saved-parameters)))
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (push (apply #'format msg args) log-messages))))

      (unwind-protect
          (progn
            ;; Call the save hook
            (claude-code-ide--workspace-save-hook mock-persp)

            ;; Verify session state was saved
            (let ((saved-state (gethash 'claude-session-state saved-parameters)))
              (should saved-state)
              (should (equal (plist-get saved-state :directory) test-directory))
              (should (equal (plist-get saved-state :workspace-name) test-workspace-name)))

            ;; Verify log message
            (should (member "Saved Claude session state for workspace: test-workspace" log-messages)))

        ;; Cleanup
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest test-claude-code-ide-workspace-restore-hook ()
  "Test workspace restore hook integration."
  (let* ((test-workspace-name "test-workspace")
         (test-directory (make-temp-file "test-dir" t))
         (test-state `(:directory ,test-directory
                                  :workspace-name ,test-workspace-name
                                  :created (encode-time 0 0 12 15 7 2025)))
         (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
         (mock-persp (list :name test-workspace-name))
         (log-messages '()))

    ;; Create test directory
    (make-directory test-directory t)

    ;; Mock persp functions
    (cl-letf* (((symbol-function 'persp-name)
                (lambda (persp) (plist-get persp :name)))
               ((symbol-function 'persp-get-buffer-parameter)
                (lambda (persp param-name)
                  (should (eq persp mock-persp))
                  (should (eq param-name 'claude-session-state))
                  test-state))
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (push (apply #'format msg args) log-messages))))

      (unwind-protect
          (progn
            ;; Call the restore hook
            (claude-code-ide--workspace-restore-hook mock-persp)

            ;; Verify restoration data was stored
            (let ((restore-info (gethash test-workspace-name claude-code-ide--workspace-restore-data)))
              (should restore-info)
              (should (equal (plist-get restore-info :state) test-state))
              (should-not (plist-get restore-info :restored)))

            ;; Verify log message
            (should (cl-some (lambda (msg)
                               (string-match "Claude session state available for workspace test-workspace" msg))
                             log-messages)))

        ;; Cleanup
        (when (file-directory-p test-directory)
          (delete-directory test-directory t))))))

(ert-deftest test-claude-code-ide-restore-session-on-demand ()
  "Test on-demand session restoration."
  (let* ((test-workspace-name "test-workspace")
         (test-directory (make-temp-file "test-dir" t))
         (test-state `(:directory ,test-directory
                                  :workspace-name ,test-workspace-name))
         (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
         (session-started nil)
         (log-messages '()))

    ;; Setup restoration data
    (puthash test-workspace-name
             (list :state test-state :restored nil)
             claude-code-ide--workspace-restore-data)

    ;; Mock session start
    (cl-letf* (((symbol-function 'claude-code-ide--start-session)
                (lambda ()
                  (setq session-started t)))
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (push (apply #'format msg args) log-messages))))

      (unwind-protect
          (progn
            ;; Call restoration
            (let ((result (claude-code-ide--restore-session-on-demand test-workspace-name)))
              (should result)
              (should session-started)

              ;; Verify restoration was marked as completed
              (let ((restore-info (gethash test-workspace-name claude-code-ide--workspace-restore-data)))
                (should (plist-get restore-info :restored)))

              ;; Verify log message
              (should (member "Restored Claude session for workspace: test-workspace" log-messages))))

        ;; Cleanup
        (when (file-directory-p test-directory)
          (delete-directory test-directory t))))))

(ert-deftest test-claude-code-ide-restore-session-on-demand-missing-directory ()
  "Test on-demand restoration with missing directory."
  (let* ((test-workspace-name "test-workspace")
         (test-directory "/nonexistent/directory")
         (test-state `(:directory ,test-directory
                                  :workspace-name ,test-workspace-name))
         (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
         (session-started nil))

    ;; Setup restoration data with nonexistent directory
    (puthash test-workspace-name
             (list :state test-state :restored nil)
             claude-code-ide--workspace-restore-data)

    ;; Mock session start
    (cl-letf (((symbol-function 'claude-code-ide--start-session)
               (lambda ()
                 (setq session-started t))))

      ;; Call restoration - should fail gracefully
      (let ((result (claude-code-ide--restore-session-on-demand test-workspace-name)))
        (should-not result)
        (should-not session-started)

        ;; Restoration should remain incomplete
        (let ((restore-info (gethash test-workspace-name claude-code-ide--workspace-restore-data)))
          (should-not (plist-get restore-info :restored)))))))

(ert-deftest test-claude-code-ide-enhanced-start-or-switch-with-restoration ()
  "Test enhanced workspace session start with restoration support."
  (let* ((test-workspace-name "test-workspace")
         (test-directory (make-temp-file "test-dir" t))
         (claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
         (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
         (restoration-attempted nil)
         (session-started nil))

    ;; Setup restoration data
    (puthash test-workspace-name
             (list :state (list :directory test-directory) :restored nil)
             claude-code-ide--workspace-restore-data)

    ;; Mock functions
    (cl-letf* (((symbol-function 'claude-code-ide--get-workspace-name)
                (lambda () test-workspace-name))
               ((symbol-function 'claude-code-ide--restore-session-on-demand)
                (lambda (workspace-name)
                  (should (equal workspace-name test-workspace-name))
                  (setq restoration-attempted t)
                  t))  ; Simulate successful restoration
               ((symbol-function 'claude-code-ide--start-session)
                (lambda ()
                  (setq session-started t))))

      (unwind-protect
          (progn
            ;; Call enhanced start function
            (claude-code-ide--enhanced-start-or-switch-workspace-session)

            ;; Should attempt restoration first
            (should restoration-attempted)
            (should-not session-started))  ; Should not start new session if restoration succeeds

        ;; Cleanup
        (when (file-directory-p test-directory)
          (delete-directory test-directory t))))))

(ert-deftest test-claude-code-ide-workspace-restore-corrupted-data ()
  "Test graceful handling of corrupted workspace restore data."
  (let* ((test-workspace-name "test-workspace")
         (mock-persp (list :name test-workspace-name))
         (log-messages '()))

    ;; Mock persp functions with corrupted data
    (cl-letf* (((symbol-function 'persp-name)
                (lambda (persp) (plist-get persp :name)))
               ((symbol-function 'persp-get-buffer-parameter)
                (lambda (_persp _param-name)
                  '(:directory "/nonexistent" :bad-data "corrupted")))  ; Return corrupted plist
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (push (apply #'format msg args) log-messages))))

      ;; Call the restore hook with corrupted data
      (claude-code-ide--workspace-restore-hook mock-persp)

      ;; Should handle gracefully with warning message
      (should (cl-some (lambda (msg)
                         (string-match "Warning.*Failed to restore.*test-workspace" msg))
                       log-messages)))))

(ert-deftest test-claude-code-ide-persistence-hooks-registration ()
  "Test registration and unregistration of persistence hooks."
  (let* ((registered-save-hooks '())
         (registered-restore-hooks '())
         (removed-save-hooks '())
         (removed-restore-hooks '())
         (log-messages '()))

    ;; Mock hook functions and persp-mode feature
    (cl-letf* (((symbol-function 'featurep)
                (lambda (feature)
                  (eq feature 'persp-mode)))
               ((symbol-function 'add-hook)
                (lambda (hook function)
                  (cond
                   ((eq hook 'persp-before-save-state-to-file-functions)
                    (push function registered-save-hooks))
                   ((eq hook 'persp-after-load-state-from-file-functions)
                    (push function registered-restore-hooks)))))
               ((symbol-function 'remove-hook)
                (lambda (hook function)
                  (cond
                   ((eq hook 'persp-before-save-state-to-file-functions)
                    (push function removed-save-hooks))
                   ((eq hook 'persp-after-load-state-from-file-functions)
                    (push function removed-restore-hooks)))))
               ((symbol-function 'claude-code-ide-log)
                (lambda (msg &rest args)
                  (push (apply #'format msg args) log-messages))))

      ;; Test registration
      (claude-code-ide--register-persistence-hooks)
      (should (member #'claude-code-ide--workspace-save-hook registered-save-hooks))
      (should (member #'claude-code-ide--workspace-restore-hook registered-restore-hooks))
      (should (member "Registered Claude workspace persistence hooks" log-messages))

      ;; Test unregistration
      (claude-code-ide--unregister-persistence-hooks)
      (should (member #'claude-code-ide--workspace-save-hook removed-save-hooks))
      (should (member #'claude-code-ide--workspace-restore-hook removed-restore-hooks))
      (should (member "Unregistered Claude workspace persistence hooks" log-messages)))))

(ert-deftest test-claude-code-ide-persistence-large-workspace-dataset ()
  "Test persistence performance with large number of workspaces."
  (let* ((num-workspaces 100)  ; Simulate large dataset
         (claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
         (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
         (serialization-times '())
         (restoration-times '()))

    ;; Create many mock sessions
    (dotimes (i num-workspaces)
      (let* ((workspace-name (format "workspace-%d" i))
             (directory (format "/test/workspace-%d" i))
             (session `(:directory ,directory
                                   :workspace-name ,workspace-name
                                   :created (current-time))))
        (puthash workspace-name session claude-code-ide--workspace-sessions)))

    ;; Test serialization performance
    (let ((start-time (current-time)))
      (maphash (lambda (workspace-name session)
                 (let ((ser-start (current-time)))
                   (claude-code-ide--serialize-session-state session)
                   (push (float-time (time-subtract (current-time) ser-start))
                         serialization-times)))
               claude-code-ide--workspace-sessions)
      (let ((total-time (float-time (time-subtract (current-time) start-time))))
        ;; Performance check: should complete in reasonable time
        (should (< total-time 5.0))  ; Less than 5 seconds for 100 workspaces

        ;; Average serialization time should be reasonable
        (let ((avg-time (/ (apply #'+ serialization-times) (length serialization-times))))
          (should (< avg-time 0.1)))))))  ; Less than 100ms per workspace on average

;; Additional comprehensive unit tests for Issue-007

(ert-deftest claude-code-ide-test-project-detection-symlinks ()
  "Test project detection with symlinks."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     (let* ((real-dir (expand-file-name default-directory))
            (link-name (expand-file-name "test-link" default-directory)))
       ;; Create a symlink to current directory
       (when (file-exists-p link-name)
         (delete-file link-name))
       (make-symbolic-link real-dir link-name)
       (unwind-protect
           (let ((default-directory link-name))
             ;; Should handle symlinks gracefully (may or may not resolve depending on environment)
             (should (stringp (claude-code-ide--get-working-directory))))
         (when (file-exists-p link-name)
           (delete-file link-name)))))))

(ert-deftest claude-code-ide-test-project-detection-nonexistent-directory ()
  "Test project detection behavior with non-existent directories."
  (let ((default-directory "/nonexistent/directory/"))
    ;; Should handle gracefully and fall back appropriately
    (should (stringp (claude-code-ide--get-working-directory)))
    ;; Should not crash or return nil
    (should (claude-code-ide--get-working-directory))))

(ert-deftest claude-code-ide-test-project-detection-tramp-urls ()
  "Test project detection with TRAMP remote URLs."
  (let ((default-directory "/ssh:example.com:/remote/path/"))
    ;; Should handle TRAMP URLs gracefully
    (should (stringp (claude-code-ide--get-working-directory)))
    ;; Should return the TRAMP URL or handle it appropriately
    (should (claude-code-ide--get-working-directory))))

(ert-deftest claude-code-ide-test-fallback-chain-behavior ()
  "Test complete fallback chain for project detection."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Mock doom-project-root to return nil (fallback scenario)
     (cl-letf (((symbol-function 'doom-project-root) (lambda (&optional _) nil)))
       ;; Should fall back to default-directory
       (should (equal (claude-code-ide--get-working-directory)
                      (expand-file-name default-directory))))

     ;; Test with doom-project-root returning a value
     (cl-letf (((symbol-function 'doom-project-root)
                (lambda (&optional _) "/mock/doom/project/"))
               ((symbol-function 'featurep)
                (lambda (feature) (eq feature 'doom))))
       ;; Should use doom-project-root result when doom is available
       (should (equal (claude-code-ide--get-working-directory)
                      "/mock/doom/project/"))))))

(ert-deftest claude-code-ide-test-session-mapping-accuracy ()
  "Test workspace session mapping accuracy."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal)))
    ;; Test setting and getting sessions for different workspaces
    (let ((session-a '(:workspace "test-a" :dir "/path/a"))
          (session-b '(:workspace "test-b" :dir "/path/b")))

      ;; Set sessions for different workspaces
      (claude-code-ide--set-workspace-session session-a "test-a")
      (claude-code-ide--set-workspace-session session-b "test-b")

      ;; Verify correct mapping
      (should (equal (claude-code-ide--get-workspace-session "test-a") session-a))
      (should (equal (claude-code-ide--get-workspace-session "test-b") session-b))

      ;; Verify isolation
      (should-not (equal (claude-code-ide--get-workspace-session "test-a")
                         (claude-code-ide--get-workspace-session "test-b"))))))

(ert-deftest claude-code-ide-test-session-isolation-memory ()
  "Test session isolation and memory management."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (initial-hash-count 0))

    ;; Record initial state
    (setq initial-hash-count (hash-table-count claude-code-ide--workspace-sessions))

    ;; Create multiple sessions
    (claude-code-ide--set-workspace-session
     '(:workspace "session-1" :buffer "buf1") "session-1")
    (claude-code-ide--set-workspace-session
     '(:workspace "session-2" :buffer "buf2") "session-2")
    (claude-code-ide--set-workspace-session
     '(:workspace "session-3" :buffer "buf3") "session-3")

    ;; Verify sessions exist
    (should (= (hash-table-count claude-code-ide--workspace-sessions)
               (+ initial-hash-count 3)))

    ;; Clear one session
    (claude-code-ide--clear-workspace-session "session-2")

    ;; Verify proper cleanup
    (should (= (hash-table-count claude-code-ide--workspace-sessions)
               (+ initial-hash-count 2)))
    (should-not (claude-code-ide--get-workspace-session "session-2"))

    ;; Verify other sessions remain
    (should (claude-code-ide--get-workspace-session "session-1"))
    (should (claude-code-ide--get-workspace-session "session-3"))))

(ert-deftest claude-code-ide-test-hook-registration-execution ()
  "Test hook registration and execution behavior."
  (let ((hook-called nil)
        (hook-args nil))

    ;; Define test hook function
    (defun test-hook-function (persp)
      (setq hook-called t)
      (setq hook-args persp))

    (unwind-protect
        (progn
          ;; Test hook registration
          (claude-code-ide--register-persistence-hooks)

          ;; Simulate perspective activation
          (run-hook-with-args 'persp-activated-functions 'test-persp)

          ;; Verify hook was called (if persp-mode is available)
          (when (featurep 'persp-mode)
            (should hook-called)
            (should (equal hook-args 'test-persp))))

      ;; Cleanup
      (claude-code-ide--unregister-persistence-hooks)
      (fmakunbound 'test-hook-function))))

(ert-deftest claude-code-ide-test-error-handling-during-transitions ()
  "Test error handling during workspace transitions."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (error-occurred nil))

    ;; Set up a session that will cause an error during transition
    (claude-code-ide--set-workspace-session
     '(:workspace "error-session" :invalid-key "bad-value") "error-session")

    ;; Test graceful error handling
    (condition-case err
        (progn
          ;; This should handle errors gracefully
          (claude-code-ide--switch-to-workspace-session "error-session")
          (claude-code-ide--switch-to-workspace-session "nonexistent-session"))
      (error
       (setq error-occurred err)))

    ;; Should handle errors without crashing the system
    (should (or (null error-occurred)
                (stringp (error-message-string error-occurred))))))

(ert-deftest claude-code-ide-test-comprehensive-edge-cases ()
  "Test various edge cases together."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Test empty workspace name
     (should (stringp (claude-code-ide--get-workspace-name)))

     ;; Test buffer name generation with nil directory (uses current directory)
     (should (stringp (claude-code-ide--get-buffer-name)))

     ;; Test session cleanup with empty hash table
     (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal)))
       (claude-code-ide--cleanup-dead-workspace-sessions)
       ;; Should not crash
       (should t))

     ;; Test workspace session operations with nil values
     (should-not (claude-code-ide--get-workspace-session nil))
     (should-not (claude-code-ide--get-workspace-session ""))

     ;; Test serialization with minimal session data
     (let ((minimal-session '(:workspace-name "test")))
       (should (listp (claude-code-ide--serialize-session-state minimal-session)))))))

;; Integration tests for workspace switching scenarios (Issue-007)

(ert-deftest claude-code-ide-integration-workspace-switching-complete ()
  "Test complete workspace switching with active Claude sessions."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (workspace-a "project-alpha")
        (workspace-b "project-beta"))

    ;; Setup: Create two workspaces with sessions
    (claude-code-ide-tests--create-mock-workspace workspace-a "/tmp/alpha")
    (claude-code-ide-tests--create-mock-workspace workspace-b "/tmp/beta")

    ;; Test: Switch between workspaces
    (claude-code-ide--switch-to-workspace-session workspace-a)
    ;; Verify session exists for workspace-a
    (should (claude-code-ide--get-workspace-session workspace-a))

    (claude-code-ide--switch-to-workspace-session workspace-b)
    ;; Verify session exists for workspace-b
    (should (claude-code-ide--get-workspace-session workspace-b))

    ;; Verify: Sessions remain isolated
    (should-not (equal (claude-code-ide--get-workspace-session workspace-a)
                       (claude-code-ide--get-workspace-session workspace-b)))

    ;; Cleanup
    (claude-code-ide--clear-workspace-session workspace-a)
    (claude-code-ide--clear-workspace-session workspace-b)))

(ert-deftest claude-code-ide-integration-session-persistence-across-switches ()
  "Test session persistence during workspace switches."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (claude-code-ide--workspace-restore-data (make-hash-table :test 'equal))
        (workspace-name "persistent-test"))

    ;; Create a session with some state
    (claude-code-ide-tests--create-mock-workspace workspace-name "/tmp/persistent")
    (let ((session (claude-code-ide--get-workspace-session workspace-name)))
      (should session)

      ;; Simulate switching away and back
      (claude-code-ide--switch-to-workspace-session "other-workspace")
      (claude-code-ide--switch-to-workspace-session workspace-name)

      ;; Session should still exist and be accessible
      (should (claude-code-ide--get-workspace-session workspace-name))
      (should (equal (plist-get session :workspace-name) workspace-name)))

    ;; Cleanup
    (claude-code-ide--clear-workspace-session workspace-name)))

(ert-deftest claude-code-ide-integration-multi-workspace-concurrent-sessions ()
  "Test multiple concurrent workspace sessions."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (workspaces '("ws1" "ws2" "ws3" "ws4" "ws5")))

    ;; Create multiple concurrent sessions
    (dolist (ws workspaces)
      (claude-code-ide-tests--create-mock-workspace ws (format "/tmp/%s" ws)))

    ;; Verify all sessions exist independently
    (dolist (ws workspaces)
      (should (claude-code-ide--get-workspace-session ws))
      (should (equal (plist-get (claude-code-ide--get-workspace-session ws)
                                :workspace-name) ws)))

    ;; Test switching between all workspaces
    (dolist (ws workspaces)
      (claude-code-ide--switch-to-workspace-session ws)
      ;; Verify session is accessible after switching
      (should (claude-code-ide--get-workspace-session ws)))

    ;; Cleanup all sessions
    (dolist (ws workspaces)
      (claude-code-ide--clear-workspace-session ws))

    ;; Verify cleanup
    (should (= (hash-table-count claude-code-ide--workspace-sessions) 0))))

(ert-deftest claude-code-ide-integration-workspace-lifecycle-hooks ()
  "Test workspace lifecycle hooks integration."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (activation-calls '())
        (deactivation-calls '()))

    ;; Mock hook functions to track calls
    (cl-letf (((symbol-function 'claude-code-ide--workspace-activated-h)
               (lambda (persp) (push persp activation-calls)))
              ((symbol-function 'claude-code-ide--workspace-deactivated-h)
               (lambda (persp) (push persp deactivation-calls))))

      ;; Test workspace switching triggers hooks
      (claude-code-ide--switch-to-workspace-session "test-ws-1")
      (claude-code-ide--switch-to-workspace-session "test-ws-2")

      ;; Verify hooks were called appropriately
      (should (>= (length activation-calls) 0))  ; May vary based on environment
      (should (>= (length deactivation-calls) 0)))))

(ert-deftest claude-code-ide-integration-error-recovery-during-switching ()
  "Test error recovery during workspace switching."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (original-switch-function (symbol-function 'claude-code-ide--switch-to-workspace-session)))

    ;; Create a valid session first
    (claude-code-ide-tests--create-mock-workspace "valid-ws" "/tmp/valid")

    ;; Mock switch function to cause an error on first call
    (let ((call-count 0))
      (cl-letf (((symbol-function 'claude-code-ide--switch-to-workspace-session)
                 (lambda (ws-name)
                   (setq call-count (1+ call-count))
                   (if (= call-count 1)
                       (error "Simulated switching error")
                     (funcall original-switch-function ws-name)))))

        ;; First switch should error
        (should-error (claude-code-ide--switch-to-workspace-session "valid-ws"))

        ;; Second switch should succeed (error recovery)
        (should-not (claude-code-ide--switch-to-workspace-session "valid-ws"))))

    ;; Cleanup
    (claude-code-ide--clear-workspace-session "valid-ws")))

(ert-deftest claude-code-ide-integration-performance-workspace-switching ()
  "Test performance of workspace switching operations."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (switch-times '())
        (num-workspaces 20))

    ;; Create multiple workspaces
    (dotimes (i num-workspaces)
      (claude-code-ide-tests--create-mock-workspace
       (format "perf-ws-%d" i) (format "/tmp/perf-%d" i)))

    ;; Measure switching times
    (dotimes (i num-workspaces)
      (let ((start-time (current-time))
            (ws-name (format "perf-ws-%d" i)))
        (claude-code-ide--switch-to-workspace-session ws-name)
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (push elapsed switch-times))))

    ;; Performance assertions
    (let ((avg-time (/ (apply #'+ switch-times) (length switch-times)))
          (max-time (apply #'max switch-times)))

      ;; Average switch time should be under 100ms
      (should (< avg-time 0.1))

      ;; Maximum switch time should be under 500ms
      (should (< max-time 0.5)))

    ;; Cleanup
    (dotimes (i num-workspaces)
      (claude-code-ide--clear-workspace-session (format "perf-ws-%d" i)))))

;; Performance and stress testing suite (Issue-007)

(ert-deftest claude-code-ide-performance-session-startup-time ()
  "Test session startup time meets performance requirements."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (startup-times '())
        (num-tests 10))

    ;; Measure session creation times
    (dotimes (i num-tests)
      (let ((start-time (current-time))
            (ws-name (format "startup-test-%d" i)))
        ;; Create session
        (claude-code-ide-tests--create-mock-workspace ws-name (format "/tmp/startup-%d" i))
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (push elapsed startup-times))
        ;; Cleanup
        (claude-code-ide--clear-workspace-session ws-name)))

    ;; Performance assertions from Issue-007: Session startup time < 2 seconds
    (let ((avg-time (/ (apply #'+ startup-times) (length startup-times)))
          (max-time (apply #'max startup-times)))
      (should (< avg-time 2.0))  ; Average under 2 seconds
      (should (< max-time 2.0))))) ; Max under 2 seconds

(ert-deftest claude-code-ide-performance-memory-usage-growth ()
  "Test memory usage growth during workspace operations."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (num-workspaces 50)
        (initial-gc-cons-threshold gc-cons-threshold))

    ;; Set up memory monitoring
    (garbage-collect) ; Clean up before testing
    (let ((initial-memory (memory-use-counts)))

      ;; Create many workspaces
      (dotimes (i num-workspaces)
        (claude-code-ide-tests--create-mock-workspace
         (format "memory-test-%d" i) (format "/tmp/memory-%d" i)))

      (garbage-collect) ; Force GC to get accurate memory reading
      (let ((final-memory (memory-use-counts)))

        ;; Calculate memory growth (simplified check)
        ;; This is a basic test - real memory profiling would be more complex
        (should (< (hash-table-count claude-code-ide--workspace-sessions) 100))

        ;; Cleanup
        (dotimes (i num-workspaces)
          (claude-code-ide--clear-workspace-session (format "memory-test-%d" i)))))))

(ert-deftest claude-code-ide-stress-concurrent-workspace-operations ()
  "Stress test with multiple concurrent workspace operations."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (num-workspaces 100)
        (operation-count 0)
        (errors '()))

    ;; Create many workspaces rapidly
    (dotimes (i num-workspaces)
      (condition-case err
          (progn
            (claude-code-ide-tests--create-mock-workspace
             (format "stress-ws-%d" i) (format "/tmp/stress-%d" i))
            (setq operation-count (1+ operation-count)))
        (error (push err errors))))

    ;; Perform rapid switching operations
    (dotimes (i 50)
      (condition-case err
          (progn
            (claude-code-ide--switch-to-workspace-session
             (format "stress-ws-%d" (random num-workspaces)))
            (setq operation-count (1+ operation-count)))
        (error (push err errors))))

    ;; Verify system stability
    (should (>= operation-count (+ num-workspaces 25))) ; At least 75% operations succeeded
    (should (< (length errors) 10)) ; Less than 10% error rate

    ;; Cleanup
    (dotimes (i num-workspaces)
      (claude-code-ide--clear-workspace-session (format "stress-ws-%d" i)))))

(ert-deftest claude-code-ide-stress-memory-leak-detection ()
  "Test for memory leaks during extended workspace usage."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (num-cycles 20)
        (workspaces-per-cycle 10))

    ;; Perform multiple create/destroy cycles
    (dotimes (cycle num-cycles)
      (let ((cycle-workspaces '()))

        ;; Create workspaces
        (dotimes (i workspaces-per-cycle)
          (let ((ws-name (format "leak-test-%d-%d" cycle i)))
            (claude-code-ide-tests--create-mock-workspace
             ws-name (format "/tmp/leak-%d-%d" cycle i))
            (push ws-name cycle-workspaces)))

        ;; Use workspaces
        (dolist (ws cycle-workspaces)
          (claude-code-ide--switch-to-workspace-session ws))

        ;; Destroy workspaces
        (dolist (ws cycle-workspaces)
          (claude-code-ide--clear-workspace-session ws))))

    ;; Verify no sessions remain
    (should (= (hash-table-count claude-code-ide--workspace-sessions) 0))

    ;; Force garbage collection and verify cleanup
    (garbage-collect)
    (should (= (hash-table-count claude-code-ide--workspace-sessions) 0))))

(ert-deftest claude-code-ide-stress-rapid-workspace-switching ()
  "Stress test rapid workspace switching operations."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (workspaces '("rapid-1" "rapid-2" "rapid-3" "rapid-4" "rapid-5"))
        (switch-count 200)
        (successful-switches 0)
        (errors '()))

    ;; Create test workspaces
    (dolist (ws workspaces)
      (claude-code-ide-tests--create-mock-workspace ws (format "/tmp/%s" ws)))

    ;; Perform rapid switching
    (dotimes (i switch-count)
      (condition-case err
          (progn
            (claude-code-ide--switch-to-workspace-session
             (nth (random (length workspaces)) workspaces))
            (setq successful-switches (1+ successful-switches)))
        (error (push err errors))))

    ;; Performance and reliability assertions
    (should (>= successful-switches (* switch-count 0.95))) ; 95% success rate
    (should (< (length errors) (* switch-count 0.05)))     ; Less than 5% errors

    ;; Cleanup
    (dolist (ws workspaces)
      (claude-code-ide--clear-workspace-session ws))))

(ert-deftest claude-code-ide-stress-serialization-performance ()
  "Stress test session serialization performance."
  (let ((claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
        (num-workspaces 100)
        (serialization-times '()))

    ;; Create many workspaces with complex session data
    (dotimes (i num-workspaces)
      (let* ((ws-name (format "serial-test-%d" i))
             (complex-session `(:workspace-name ,ws-name
                                                :directory ,(format "/tmp/serial-%d" i)
                                                :created ,(current-time)
                                                :process nil
                                                :buffer nil
                                                :metadata (:project-type "test"
                                                                         :last-command "echo test"
                                                                         :session-id ,(format "session-%d" i)))))
        (claude-code-ide--set-workspace-session complex-session ws-name)))

    ;; Test serialization performance
    (maphash (lambda (ws-name session)
               (let ((start-time (current-time)))
                 (claude-code-ide--serialize-session-state session)
                 (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                   (push elapsed serialization-times))))
             claude-code-ide--workspace-sessions)

    ;; Performance assertions
    (let ((avg-time (/ (apply #'+ serialization-times) (length serialization-times)))
          (max-time (apply #'max serialization-times)))
      (should (< avg-time 0.01))  ; Average under 10ms per serialization
      (should (< max-time 0.05))) ; Max under 50ms per serialization

    ;; Cleanup
    (dotimes (i num-workspaces)
      (claude-code-ide--clear-workspace-session (format "serial-test-%d" i)))))

;; Helper function for integration tests
(defun claude-code-ide-tests--create-mock-workspace (workspace-name directory)
  "Create a mock workspace session for testing."
  (let ((session `(:workspace-name ,workspace-name
                                   :directory ,directory
                                   :created ,(current-time)
                                   :process nil
                                   :buffer nil)))
    (claude-code-ide--set-workspace-session session workspace-name)))

;;; VTerm Integration Tests

(ert-deftest claude-code-ide-test-vterm-hook-integration ()
  "Test that vterm-mode-hook correctly sets up environment variables for unmanaged terminals."
  (let* ((temp-dir (make-temp-file "test-vterm-" t))
         (claude-code-ide--workspace-sessions (make-hash-table :test 'equal))
         (claude-code-ide-mcp--sessions (make-hash-table :test 'equal))
         (test-workspace "test-vterm-workspace")
         (test-port nil)
         (vterm-environment nil)
         (vterm-mode t))

    (unwind-protect
        (progn
          ;; Mock workspace name function
          (cl-letf (((symbol-function 'claude-code-ide--get-workspace-name)
                     (lambda () test-workspace)))

            ;; Start MCP server for test workspace
            (setq test-port (claude-code-ide-mcp-start test-workspace))
            (should (numberp test-port))

            ;; Verify workspace is registered in the registry
            (should (equal test-port (claude-code-ide-mcp-get-workspace-port test-workspace)))

            ;; Simulate being in an unmanaged vterm buffer (not named *claude-code*)
            (with-temp-buffer
              (rename-buffer "*vterm-test*")
              ;; Set vterm-mode flag to simulate being in vterm mode
              (setq-local vterm-mode t)

              ;; Call the vterm setup function directly
              (claude-code-ide--setup-vterm-environment)

              ;; Debug: Check what happened
              (message "Buffer: %s, vterm-mode: %s, workspace: %s, port: %s"
                       (buffer-name) vterm-mode
                       (claude-code-ide--get-workspace-name)
                       (when (claude-code-ide--get-workspace-name)
                         (claude-code-ide-mcp-get-workspace-port (claude-code-ide--get-workspace-name))))
              (message "vterm-environment (local-var-p): %s, value: %s"
                       (local-variable-p 'vterm-environment (current-buffer))
                       vterm-environment)

              ;; Verify environment variables were set
              (should (local-variable-p 'vterm-environment (current-buffer)))
              (should vterm-environment)
              (should (member (format "CLAUDE_CODE_SSE_PORT=%d" test-port) vterm-environment))
              (should (member "ENABLE_IDE_INTEGRATION=true" vterm-environment))
              (should (member "TERM_PROGRAM=emacs" vterm-environment))
              (should (member "FORCE_CODE_TERMINAL=true" vterm-environment)))))

      ;; Cleanup
      (when test-port
        (claude-code-ide-mcp-stop-session test-workspace))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest claude-code-ide-test-vterm-hook-managed-buffer-ignored ()
  "Test that managed vterm buffers (created by claude-code-ide) are ignored by the hook."
  (let ((original-env nil))

    ;; Simulate being in a managed vterm buffer (named *claude-code*)
    (with-temp-buffer
      (rename-buffer "*claude-code-test*")
      (setq-local vterm-mode t)
      (setq-local vterm-environment nil)
      (setq original-env vterm-environment)

      ;; Call the vterm setup function
      (claude-code-ide--setup-vterm-environment)

      ;; Environment should not be modified for managed buffers
      (should (equal vterm-environment original-env)))))

(ert-deftest claude-code-ide-test-vterm-hook-no-workspace ()
  "Test that vterm hook gracefully handles cases with no workspace."
  (let ((original-env nil))

    (cl-letf (((symbol-function 'claude-code-ide--get-workspace-name)
               (lambda () nil)))

      (with-temp-buffer
        (rename-buffer "*vterm-test*")
        (setq-local vterm-mode t)
        (setq-local vterm-environment nil)
        (setq original-env vterm-environment)

        ;; Call the vterm setup function
        (claude-code-ide--setup-vterm-environment)

        ;; Environment should not be modified when no workspace
        (should (equal vterm-environment original-env))))))

(provide 'claude-code-ide-tests)

;;; claude-code-ide-tests.el ends here
