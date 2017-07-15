;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-symbol)
(require 'func-package)
(require 'func-path)
(require 'func-eval)

(defvar serika-tasks '()
  "List of tasks.
Task format is `foo+bar'. Actual task dir is `serika-task-directory'/foo/bar.")

(defvar serika-executed-tasks '()
  "List of executed task.")

(defconst serika-task-delimiter "+"
  "Task delimiter.")

(defun serika-c/task/exists (task)
  "Return t if TASK is exists."
  (numberp (cl-position task serika-tasks)))

(defun serika-c/task/add (task)
  "Add TASK to `serika-tasks'."
  (add-to-list 'serika-tasks task t))

(defun serika-c/task/parse-dir (task)
  "Get path to directory of TASK."
  (cl-reduce 'serika-f/path/join (split-string task serika-task-delimiter)))

(defun serika-c/task/execute (task &optional parent-task)
  "Execute subtask TASK of PARENT-TASK."
  (let* ((true-task (if (string= "+" (substring (serika-f/symbol/name task) 0 1))
                        (serika-f/symbol/concat parent-task task)
                      task))
         (true-task-name (serika-f/symbol/name true-task))
         (task-subdir (serika-c/task/parse-dir true-task-name))
         (deps-path (serika-f/path/join serika-task-directory
                                      task-subdir
                                      "deps.el"))
         (packages-path (serika-f/path/join serika-task-directory
                                          task-subdir
                                          "packages.el"))
         (vars-path (serika-f/path/join serika-task-directory
                                      task-subdir
                                      "vars.el"))
         (funcs-path (serika-f/path/join serika-task-directory
                                       task-subdir
                                       "funcs.el")))
    (unless (numberp (cl-position true-task-name serika-executed-tasks :test 'equal))
      (progn
        (add-to-list 'serika-executed-tasks true-task-name t)
        (when (file-exists-p packages-path)
          (dolist (package-name (serika-f/eval/file packages-path))
            (serika-f/package/make-sure-installed package-name)))
        (when (file-exists-p deps-path)
          (dolist (dep (serika-f/eval/file deps-path))
            (serika-c/task/execute dep true-task)))
        (when (file-exists-p vars-path)
          (serika-f/eval/file vars-path))
        (when (file-exists-p funcs-path)
          ;; save `init' function to `--temp-init' variable if any
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
            (fmakunbound '--temp-init)))))))

(defun serika-c/task/execute-all ()
  "Execute all task defined by `serika-tasks'."
  (mapcar 'serika-c/task/execute serika-tasks))

(provide 'core-task)
;;; core-task.el ends here
