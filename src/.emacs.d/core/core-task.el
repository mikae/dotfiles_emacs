;;; package --- Summary
;;; Commentary:
;;; Code:

;; (require 'core-util)

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
    (cl-flet ((--load-funcs (file)
                            (when (fboundp 'init)
                              (fset '--temp-init 'init)
                              (fmakunbound 'init))
                            (--eval-file file)
                            (when (fboundp 'init)
                              (progn
                                (init)
                                (fmakunbound 'init)))
                            ;; restore `init' function
                            (when (fboundp '--temp-init)
                              (fset 'init '--temp-init)
                              (fmakunbound '--temp-init))))
      (let* ((module-path (util/path-join serika-task-directory
                                          task
                                          "module"))
             (funcs-path (util/path-join serika-task-directory
                                         task
                                         "funcs.el"))
             (funcs-path-compiled (util/path-join serika-task-directory
                                                  task
                                                  "funcs.elc")))
        (cond
         ((file-exists-p funcs-path)
          (--load-funcs funcs-path))
         ((file-exists-p funcs-path-compiled)
          (--load-funcs funcs-path-compiled)))

        (let ((--temporary-tasks ())
              (--module-info     nil)
              (--temp))
          (dolist (dir (directory-files (util/path-join serika-task-directory
                                                        task)
                                        t))
            (when (and (file-directory-p dir)
                       (not (string-match "\/\\.$" dir))
                       (not (string-match "\/\\.\\.$" dir)))
              (add-to-list '--temporary-tasks
                           (util/path-join task
                                           (file-name-nondirectory (directory-file-name dir))))))

          ;; todo: describe code below
          (when (file-exists-p module-path)
            (setq --module-info
                  (split-string (util/read-file-as-string module-path)))
            (cl-loop for --task in --module-info
                     do
                     (setq --temp (util/path-join task
                                                  --task))
                     (when (cl-member --temp --temporary-tasks
                                      :test 'equal)
                       ;; todo: somehow avoid copying of `--temporary-tasks'
                       (setq --temporary-tasks
                             (cons --temp (delete --temp --temporary-tasks))))))

          (setq serika-tasks (nconc --temporary-tasks serika-tasks)))))))

(defun serika-c/task/execute-all ()
  "Execute all task defined by `serika-tasks'."
  (while (> (length serika-tasks) 0)
    (serika-c/task/execute (pop serika-tasks))))

(provide 'core-task)
;;; core-task.el ends here
