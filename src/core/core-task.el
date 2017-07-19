;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-symbol)
(require 'func-path)
(require 'func-eval)

(defvar serika-tasks (list)
  "List of tasks that will be executed.")

(defun serika-c/task/exists (task)
  "Return t if TASK is exists."
  (numberp (cl-position task serika-tasks)))

(defun serika-c/task/add (task)
  "Add TASK to `serika-tasks'."
  (add-to-list 'serika-tasks task t))

(defun serika-c/task/parse-dir (task)
  "Get path to directory of TASK."
  (cl-reduce 'serika-f/path/join (split-string task serika-task-delimiter)))

(defun serika-c/task/execute (task)
  "Execute subtask TASK of PARENT-TASK."
  (let* ((vars-path  (serika-f/path/join serika-task-directory
                                         task
                                         "vars.el"))
         (funcs-path (serika-f/path/join serika-task-directory
                                         task
                                         "funcs.el")))
    (when (file-exists-p vars-path)
      (message "vars-path")
      (serika-f/eval/file vars-path))
    (when (file-exists-p funcs-path)
      (message "funcs-path")
      (when (fboundp 'init)
        (fset '--temp-init 'init)
        (fmakunbound 'init))
      (serika-f/eval/file funcs-path)
      (when (fboundp 'init)
        (progn
          (init)
          (fmakunbound 'init)))
      ;; restore `init' function
      (when (fboundp '--temp-init)
        (fset 'init '--temp-init)
        (fmakunbound '--temp-init)))
    (dolist (dir (directory-files (serika-f/path/join serika-task-directory
                                                      task)
                                  t))
      (if (and (file-directory-p dir)
                 (not (string-match "\/\\.$" dir))
                 (not (string-match "\/\\.\\.$" dir)))
					(add-to-list 'serika-tasks (serika-f/path/join task
																												 (file-name-nondirectory (directory-file-name dir))))
				(when (file-directory-p dir)
					(message dir))
				))))

(defun serika-c/task/execute-all ()
  "Execute all task defined by `serika-tasks'."
  (while (> (length serika-tasks) 0)
    (serika-c/task/execute (pop serika-tasks))))

(provide 'core-task)
;;; core-task.el ends here
