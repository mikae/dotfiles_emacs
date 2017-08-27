;;; package --- Summary
;;; Commentary:
;;; Code:

;; Vars
(defvar serika-tasks (list)
  "List of tasks that will be executed.")

;; Funcs
(defun serika-c/task/add (task)
  "Add TASK to `serika-tasks'."
  (add-to-list 'serika-tasks task t))

(defun serika-c/task/execute (task)
  "Execute subtask TASK of PARENT-TASK."
  (cl-flet ((--eval-file (file)
                         (load-file file)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (emacs-lisp-mode)
                           (goto-char (point-max))
                           (backward-sexp)
                           (eval (sexp-at-point)))))
    (let* ((funcs-path (func/path-join serika-task-directory
                                       task
                                       "funcs.el")))
      (when (file-exists-p funcs-path)
        (message "funcs-path")
        (when (fboundp 'init)
          (fset '--temp-init 'init)
          (fmakunbound 'init))
        (--eval-file funcs-path)
        (when (fboundp 'init)
          (progn
            (init)
            (fmakunbound 'init)))
        ;; restore `init' function
        (when (fboundp '--temp-init)
          (fset 'init '--temp-init)
          (fmakunbound '--temp-init)))
      (dolist (dir (directory-files (func/path-join serika-task-directory
                                                    task)
                                    t))
        (if (and (file-directory-p dir)
                 (not (string-match "\/\\.$" dir))
                 (not (string-match "\/\\.\\.$" dir)))
            (add-to-list 'serika-tasks (func/path-join task
                                                       (file-name-nondirectory (directory-file-name dir))))
          (when (file-directory-p dir)
            (message dir)))))))

(defun serika-c/task/execute-all ()
  "Execute all task defined by `serika-tasks'."
  (while (> (length serika-tasks) 0)
    (serika-c/task/execute (pop serika-tasks))))

(provide 'core-task)
;;; core-task.el ends here
