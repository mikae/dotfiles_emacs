;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-symbol)
(require 'func-package)
(require 'func-misc)
(require 'func-path)

(defvar serika-tasks '()
  "List of tasks.
Task format is `foo+bar'. Actual task dir is `serika-task-directory'/foo/bar.")

(defvar serika-executed-tasks '()
  "List of executed task.")

(defconst serika-task-delimiter "+"
  "Task delimiter.")

(defun serika/task/exists (task)
  "Return t if TASK is exists."
  (numberp (cl-position task serika-tasks)))

(defun serika/task/add (task)
  "Add TASK to `serika-tasks'."
  (add-to-list 'serika-tasks task t))

(defun serika/task/parse-dir (task)
  "Get path to directory of TASK."
  (cl-reduce 'serika/path/join (split-string task serika-task-delimiter)))

(defun serika/task/execute (task &optional parent-task)
  "Execute subtask TASK of PARENT-TASK."
  (let* ((true-task (if (string= "+" (substring (serika/symbol/name task) 0 1))
                        (serika/symbol/concat parent-task task)
                      task))
         (true-task-name (serika/symbol/name true-task))
         (task-subdir (serika/task/parse-dir true-task-name))
         (config-path (serika/path/join serika-task-directory
                                        task-subdir
                                        "config.el"))
         (deps-path (serika/path/join serika-task-directory
                                      task-subdir
                                      "deps.el"))
         (funcs-path (serika/path/join serika-task-directory
                                       task-subdir
                                       "funcs.el"))
         (vars-path (serika/path/join serika-task-directory
                                      task-subdir
                                      "vars.el"))
         (packages-path (serika/path/join serika-task-directory
                                      task-subdir
                                      "packages.el"))
         )
    (unless (numberp (cl-position true-task-name serika-executed-tasks :test 'equal))
      (progn
        (add-to-list 'serika-executed-tasks true-task-name t)
        (when (file-exists-p deps-path)
          (dolist (dep (serika/misc/eval-file deps-path))
            (serika/task/execute dep true-task)))
        (when (file-exists-p packages-path)
          (dolist (package-name (serika/misc/eval-file packages-path))
            (serika/package/make-sure-installed package-name)))
        (when (file-exists-p vars-path)
          (serika/misc/eval-file vars-path))
        (when (file-exists-p funcs-path)
          (serika/misc/eval-file funcs-path))
        (when (file-exists-p config-path)
          (serika/misc/eval-file config-path))))))

(defun serika/task/execute-all ()
  "Execute all task defined by `serika-tasks'."
  (mapcar 'serika/task/execute serika-tasks))

(provide 'core-task)
;;; core-task.el ends here
