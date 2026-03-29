;;; bettermise.el --- Emacs interface for the mise build tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Endi Sukaj

;; Author: Endi Sukaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.4.0"))
;; Keywords: tools, processes, build
;; URL: https://github.com/esukaj/bettermise

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Bettermise provides an Emacs interface for mise (https://mise.jdx.dev/),
;; the polyglot dev tool manager.  It lets you:
;;
;; - List, install, and remove tool versions
;; - Run and manage tasks
;; - View and set environment variables
;; - Inspect project configuration
;;
;; Entry point: `M-x bettermise'

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'transient)

;; ──────────────────────────────────────────────────────────────────
;; Customization
;; ──────────────────────────────────────────────────────────────────

(defgroup bettermise nil
  "Emacs interface for the mise build tool."
  :group 'tools
  :prefix "bettermise-")

(defcustom bettermise-executable "mise"
  "Path to the mise executable."
  :type 'string
  :group 'bettermise)

(defcustom bettermise-project-directory nil
  "Project directory for mise commands.
When nil, use `default-directory'."
  :type '(choice (const nil) directory)
  :group 'bettermise)

;; ──────────────────────────────────────────────────────────────────
;; Faces
;; ──────────────────────────────────────────────────────────────────

(defface bettermise-tool-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool names."
  :group 'bettermise)

(defface bettermise-version
  '((t :inherit font-lock-constant-face))
  "Face for version strings."
  :group 'bettermise)

(defface bettermise-task-name
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for task names."
  :group 'bettermise)

(defface bettermise-section-header
  '((t :inherit font-lock-comment-face :weight bold :height 1.1))
  "Face for section headers."
  :group 'bettermise)

(defface bettermise-active
  '((t :inherit success))
  "Face for active/installed indicators."
  :group 'bettermise)

(defface bettermise-inactive
  '((t :inherit warning))
  "Face for inactive/missing indicators."
  :group 'bettermise)

;; ──────────────────────────────────────────────────────────────────
;; Internals
;; ──────────────────────────────────────────────────────────────────

(defun bettermise--project-dir ()
  "Return the project directory for mise commands."
  (or bettermise-project-directory default-directory))

(defun bettermise--run-command (args &optional callback)
  "Run mise with ARGS synchronously and return output string.
If CALLBACK is non-nil, run asynchronously and call CALLBACK with output."
  (let ((default-directory (bettermise--project-dir)))
    (if callback
        (let ((buf (generate-new-buffer " *bettermise-async*")))
          (set-process-sentinel
           (start-process "bettermise" buf bettermise-executable
                          (split-string args))
           (lambda (proc _event)
             (when (eq (process-status proc) 'exit)
               (with-current-buffer (process-buffer proc)
                 (funcall callback (buffer-string)))
               (kill-buffer (process-buffer proc))))))
      (with-temp-buffer
        (let ((exit-code (apply #'call-process bettermise-executable nil t nil
                                (split-string-and-unquote args))))
          (if (zerop exit-code)
              (buffer-string)
            (error "mise %s failed (exit %d): %s" args exit-code (buffer-string))))))))

(defun bettermise--run-command-to-buffer (buffer-name args)
  "Run mise ARGS in a compilation-like buffer named BUFFER-NAME."
  (let ((default-directory (bettermise--project-dir))
        (cmd (format "%s %s" bettermise-executable args)))
    (compile cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer buffer-name t))))

(defun bettermise--parse-json (args)
  "Run mise ARGS and parse JSON output."
  (let ((output (bettermise--run-command args)))
    (json-parse-string output :object-type 'alist :array-type 'list)))

;; ──────────────────────────────────────────────────────────────────
;; Tools (ls / install / uninstall / use)
;; ──────────────────────────────────────────────────────────────────

(defvar bettermise-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'bettermise-tools-refresh)
    (define-key map (kbd "i")   #'bettermise-tool-install)
    (define-key map (kbd "d")   #'bettermise-tool-uninstall)
    (define-key map (kbd "u")   #'bettermise-tool-use)
    (define-key map (kbd "U")   #'bettermise-tool-upgrade)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "RET") #'bettermise-tool-info)
    map)
  "Keymap for `bettermise-tools-mode'.")

(define-derived-mode bettermise-tools-mode special-mode "Bettermise-Tools"
  "Major mode for viewing mise tools.

\\{bettermise-tools-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun bettermise--tool-at-point ()
  "Return the tool@version string for the current line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  \\([^ ]+\\)\\s-+\\([^ ]+\\)")
      (cons (match-string 1) (match-string 2)))))

(defun bettermise-tools-refresh ()
  "Refresh the tools list."
  (interactive)
  (bettermise-tools))

(defun bettermise-tools ()
  "Display installed and active mise tools."
  (interactive)
  (let* ((output (bettermise--run-command "ls --json"))
         (tools (json-parse-string output :object-type 'alist :array-type 'list))
         (buf (get-buffer-create "*bettermise-tools*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "mise Tools" 'face 'bettermise-section-header) "\n")
        (insert (propertize (format "Directory: %s" (bettermise--project-dir))
                            'face 'font-lock-comment-face)
                "\n\n")
        (insert (propertize (format "  %-20s %-15s %-12s %s" "Tool" "Version" "Status" "Source")
                            'face 'bold)
                "\n")
        (insert "  " (make-string 70 ?─) "\n")
        (if (null tools)
            (insert "  No tools configured.\n")
          (dolist (tool tools)
            (let* ((name (alist-get 'name tool))
                   (version (or (alist-get 'version tool) "N/A"))
                   (installed (alist-get 'installed tool))
                   (active (alist-get 'active tool))
                   (source-alist (alist-get 'source tool))
                   (source (if source-alist
                               (or (alist-get 'path source-alist) "")
                             ""))
                   (status (cond
                            ((and installed active) "active")
                            (installed "installed")
                            (t "missing")))
                   (status-face (if (string= status "missing")
                                    'bettermise-inactive
                                  'bettermise-active)))
              (insert (format "  %-20s %-15s %-12s %s\n"
                              (propertize name 'face 'bettermise-tool-name)
                              (propertize version 'face 'bettermise-version)
                              (propertize status 'face status-face)
                              (propertize (if (stringp source) source "")
                                          'face 'font-lock-comment-face))))))
        (insert "\n"
                (propertize "Keys: " 'face 'bold)
                "i:install  d:uninstall  u:use  U:upgrade  g:refresh  q:quit\n"))
      (bettermise-tools-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun bettermise-tool-install (tool-version)
  "Install TOOL-VERSION (e.g. \"node@20\" or \"python\")."
  (interactive "sTool[@version] to install: ")
  (bettermise--run-command-to-buffer "*bettermise-install*"
                                     (format "install %s" tool-version)))

(defun bettermise-tool-uninstall (tool-version)
  "Uninstall TOOL-VERSION."
  (interactive
   (let ((tv (bettermise--tool-at-point)))
     (list (read-string "Tool@version to uninstall: "
                        (when tv (format "%s@%s" (car tv) (cdr tv)))))))
  (bettermise--run-command-to-buffer "*bettermise-uninstall*"
                                     (format "uninstall %s" tool-version)))

(defun bettermise-tool-use (tool-version)
  "Activate TOOL-VERSION in the current project."
  (interactive "sTool[@version] to use: ")
  (bettermise--run-command-to-buffer "*bettermise-use*"
                                     (format "use %s" tool-version)))

(defun bettermise-tool-upgrade ()
  "Interactively upgrade tools."
  (interactive)
  (bettermise--run-command-to-buffer "*bettermise-upgrade*" "up --interactive"))

(defun bettermise-tool-info ()
  "Show info for tool at point."
  (interactive)
  (let ((tv (bettermise--tool-at-point)))
    (unless tv (user-error "No tool at point"))
    (let ((output (bettermise--run-command (format "ls-remote %s" (car tv)))))
      (with-current-buffer (get-buffer-create "*bettermise-tool-info*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Available versions for %s" (car tv))
                              'face 'bettermise-section-header)
                  "\n\n")
          (insert output))
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer "*bettermise-tool-info*"))))

;; ──────────────────────────────────────────────────────────────────
;; Tasks (list / run / watch)
;; ──────────────────────────────────────────────────────────────────

(defvar bettermise-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'bettermise-tasks-refresh)
    (define-key map (kbd "RET") #'bettermise-task-run-at-point)
    (define-key map (kbd "w")   #'bettermise-task-watch-at-point)
    (define-key map (kbd "e")   #'bettermise-task-edit-at-point)
    (define-key map (kbd "I")   #'bettermise-task-info-at-point)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `bettermise-tasks-mode'.")

(define-derived-mode bettermise-tasks-mode special-mode "Bettermise-Tasks"
  "Major mode for viewing mise tasks.

\\{bettermise-tasks-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun bettermise--task-at-point ()
  "Return the task name on the current line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  \\([^ ]+\\)")
      (match-string 1))))

(defun bettermise-tasks-refresh ()
  "Refresh the tasks list."
  (interactive)
  (bettermise-tasks))

(defun bettermise-tasks ()
  "Display available mise tasks."
  (interactive)
  (let* ((output (bettermise--run-command "tasks ls --json"))
         (tasks (json-parse-string output :object-type 'alist :array-type 'list))
         (buf (get-buffer-create "*bettermise-tasks*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "mise Tasks" 'face 'bettermise-section-header) "\n")
        (insert (propertize (format "Directory: %s" (bettermise--project-dir))
                            'face 'font-lock-comment-face)
                "\n\n")
        (insert (propertize (format "  %-25s %-15s %s" "Task" "Source" "Description")
                            'face 'bold)
                "\n")
        (insert "  " (make-string 70 ?─) "\n")
        (if (null tasks)
            (insert "  No tasks found.\n")
          (dolist (task tasks)
            (let ((name (or (alist-get 'name task) ""))
                  (source (or (alist-get 'source task) ""))
                  (desc (or (alist-get 'description task) "")))
              (insert (format "  %-25s %-15s %s\n"
                              (propertize name 'face 'bettermise-task-name)
                              (propertize source 'face 'font-lock-comment-face)
                              desc)))))
        (insert "\n"
                (propertize "Keys: " 'face 'bold)
                "RET:run  w:watch  e:edit  I:info  g:refresh  q:quit\n"))
      (bettermise-tasks-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun bettermise-task-run (task &optional args)
  "Run a mise TASK with optional ARGS."
  (interactive
   (let* ((tasks-output (bettermise--run-command "tasks ls --json"))
          (tasks (json-parse-string tasks-output :object-type 'alist :array-type 'list))
          (names (mapcar (lambda (t) (alist-get 'name t)) tasks))
          (task (completing-read "Task to run: " names nil t)))
     (list task (read-string (format "Args for %s: " task)))))
  (let ((cmd (if (and args (not (string-empty-p args)))
                 (format "run %s -- %s" task args)
               (format "run %s" task))))
    (bettermise--run-command-to-buffer
     (format "*bettermise-run[%s]*" task) cmd)))

(defun bettermise-task-run-at-point ()
  "Run the task at point."
  (interactive)
  (let ((task (bettermise--task-at-point)))
    (unless task (user-error "No task at point"))
    (let ((args (read-string (format "Args for %s (empty for none): " task))))
      (bettermise-task-run task args))))

(defun bettermise-task-watch-at-point ()
  "Watch the task at point."
  (interactive)
  (let ((task (bettermise--task-at-point)))
    (unless task (user-error "No task at point"))
    (bettermise--run-command-to-buffer
     (format "*bettermise-watch[%s]*" task)
     (format "watch %s" task))))

(defun bettermise-task-edit-at-point ()
  "Edit the task at point using $EDITOR or `find-file'."
  (interactive)
  (let ((task (bettermise--task-at-point)))
    (unless task (user-error "No task at point"))
    ;; Try to find the task file
    (let ((info-output (ignore-errors (bettermise--run-command (format "tasks info %s" task)))))
      (if (and info-output (string-match "source:\\s-*\\(.+\\)" info-output))
          (find-file (string-trim (match-string 1 info-output)))
        ;; Fallback: open mise.toml
        (let ((toml (expand-file-name "mise.toml" (bettermise--project-dir))))
          (if (file-exists-p toml)
              (find-file toml)
            (user-error "Cannot determine task source")))))))

(defun bettermise-task-info-at-point ()
  "Show info for task at point."
  (interactive)
  (let ((task (bettermise--task-at-point)))
    (unless task (user-error "No task at point"))
    (let ((output (bettermise--run-command (format "tasks info %s" task))))
      (with-current-buffer (get-buffer-create "*bettermise-task-info*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Task: %s" task)
                              'face 'bettermise-section-header)
                  "\n\n")
          (insert output))
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer "*bettermise-task-info*"))))

;; ──────────────────────────────────────────────────────────────────
;; Environment
;; ──────────────────────────────────────────────────────────────────

(defun bettermise-env ()
  "Display the environment variables mise would set."
  (interactive)
  (let ((output (bettermise--run-command "env"))
        (buf (get-buffer-create "*bettermise-env*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "mise Environment" 'face 'bettermise-section-header) "\n")
        (insert (propertize (format "Directory: %s" (bettermise--project-dir))
                            'face 'font-lock-comment-face)
                "\n\n")
        (insert output))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun bettermise-set-env (key value)
  "Set environment variable KEY=VALUE in mise.toml."
  (interactive "sVariable name: \nsValue: ")
  (let ((output (bettermise--run-command (format "set %s=%s" key value))))
    (message "Set %s=%s %s" key value (string-trim output))))

;; ──────────────────────────────────────────────────────────────────
;; Config / Doctor / Settings
;; ──────────────────────────────────────────────────────────────────

(defun bettermise-doctor ()
  "Run `mise doctor' and display diagnostics."
  (interactive)
  (let ((output (bettermise--run-command "doctor"))
        (buf (get-buffer-create "*bettermise-doctor*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "mise Doctor" 'face 'bettermise-section-header) "\n\n")
        (insert (ansi-color-apply output)))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun bettermise-settings ()
  "Display current mise settings."
  (interactive)
  (let ((output (bettermise--run-command "settings"))
        (buf (get-buffer-create "*bettermise-settings*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "mise Settings" 'face 'bettermise-section-header) "\n\n")
        (insert output))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun bettermise-open-config ()
  "Open the project's mise.toml file."
  (interactive)
  (let ((toml (expand-file-name "mise.toml" (bettermise--project-dir))))
    (if (file-exists-p toml)
        (find-file toml)
      ;; Try .mise.toml
      (let ((dot-toml (expand-file-name ".mise.toml" (bettermise--project-dir))))
        (if (file-exists-p dot-toml)
            (find-file dot-toml)
          (when (y-or-n-p "No mise.toml found. Create one? ")
            (find-file toml)))))))

;; ──────────────────────────────────────────────────────────────────
;; Exec
;; ──────────────────────────────────────────────────────────────────

(defun bettermise-exec (command)
  "Execute COMMAND within the mise environment."
  (interactive "sCommand to run in mise env: ")
  (bettermise--run-command-to-buffer "*bettermise-exec*"
                                     (format "exec -- %s" command)))

;; ──────────────────────────────────────────────────────────────────
;; Transient menus (main entry point)
;; ──────────────────────────────────────────────────────────────────

(transient-define-prefix bettermise ()
  "Mise build tool interface."
  ["Tools"
   ("t" "List tools"      bettermise-tools)
   ("i" "Install tool"    bettermise-tool-install)
   ("u" "Use tool"        bettermise-tool-use)
   ("U" "Upgrade tools"   bettermise-tool-upgrade)]
  ["Tasks"
   ("r" "Run task"        bettermise-task-run)
   ("T" "List tasks"      bettermise-tasks)]
  ["Environment & Config"
   ("e" "Show env"        bettermise-env)
   ("s" "Set env var"     bettermise-set-env)
   ("c" "Open config"     bettermise-open-config)
   ("S" "Settings"        bettermise-settings)]
  ["Diagnostics"
   ("d" "Doctor"          bettermise-doctor)
   ("x" "Exec command"    bettermise-exec)])

(provide 'bettermise)

;;; bettermise.el ends here
