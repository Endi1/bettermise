;;; bettermise.el --- Emacs interface for the mise build tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Endi Sukaj

;; Author: Endi Sukaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.4.0"))
;; Keywords: tools, processes, build
;; URL: https://github.com/Endi1/bettermise

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Bettermise provides an Emacs interface for mise (https://mise.jdx.dev/),
;; the polyglot dev tool manager.  It lets you:
;;
;; - Run and manage tasks
;; - View and set environment variables
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

(defface bettermise-task-name
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for task names."
  :group 'bettermise)

(defface bettermise-section-header
  '((t :inherit font-lock-comment-face :weight bold :height 1.1))
  "Face for section headers."
  :group 'bettermise)

;; ──────────────────────────────────────────────────────────────────
;; Internals
;; ──────────────────────────────────────────────────────────────────

(defun bettermise--project-dir ()
  "Return the project directory for mise commands."
  (or bettermise-project-directory default-directory))

(defun bettermise--run-command (args)
  "Run mise with ARGS synchronously and return output string."
  (let ((default-directory (bettermise--project-dir)))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process bettermise-executable nil t nil
                              (split-string-and-unquote args))))
        (if (zerop exit-code)
            (buffer-string)
          (error "mise %s failed (exit %d): %s" args exit-code (buffer-string)))))))

(defun bettermise--run-command-to-buffer (buffer-name args)
  "Run mise ARGS in a compilation-like buffer named BUFFER-NAME."
  (let ((default-directory (bettermise--project-dir))
        (cmd (format "%s %s" bettermise-executable args)))
    (compile cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer buffer-name t))))

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
  "Edit the task at point."
  (interactive)
  (let ((task (bettermise--task-at-point)))
    (unless task (user-error "No task at point"))
    (let ((info-output (ignore-errors (bettermise--run-command (format "tasks info %s" task)))))
      (if (and info-output (string-match "source:\\s-*\\(.+\\)" info-output))
          (find-file (string-trim (match-string 1 info-output)))
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
;; Transient menu (main entry point)
;; ──────────────────────────────────────────────────────────────────

(transient-define-prefix bettermise ()
  "Mise build tool interface."
  ["Tasks"
   ("r" "Run task"        bettermise-task-run)
   ("T" "List tasks"      bettermise-tasks)]
  ["Environment"
   ("e" "Show env"        bettermise-env)
   ("s" "Set env var"     bettermise-set-env)])

(provide 'bettermise)

;;; bettermise.el ends here
