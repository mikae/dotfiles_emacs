;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'core-package)
(require '--execution-graph)
(require 'core-funcs)
(require 'url)

(defvar --serika-execution-graph nil
  "Execution graph for all tasks.")

(defun serika-c/eg/create (&rest packages)
  (setq --serika-execution-graph (eg/create))

  (dolist (task '(
                  ;; pretasks
                  "-inf"

                  ;; zero tasks
                  "zero require"
                  "zero configure"
                  "zero lib install"
                  "zero lib require"

                  ;; base tasks
                  "base require"
                  "base configure"
                  "base interface"
                  "base post install"
                  "base post require"

                  ;; tasks
                  "install"
                  "require"
                  "interface"
                  "settings"
                  "keymap"
                  "global-keymap"
                  "hook"
                  "post"
                  "post activate"

                  ;; end tasks
                  "inf"))
    (eg/create-path --serika-execution-graph task)))

(cl-defun serika-c/eg/add (&key (parents nil)
                                (name    '__unnamed__)
                                (func    nil)
                                (node    nil node-p))
  "Add new execution node."
  (if node-p
      (eg/add --serika-execution-graph
              :parents parents
              :node    node)
    (eg/add --serika-execution-graph
            :parents parents
            :name    name
            :func    func)))

(cl-defun serika-c/eg/add-install (&key (type         'package)
                                        (package-list '())
                                        (name         '__unnamed__)
                                        (parents      nil)
                                        (src          nil)
                                        (post-hook    nil)
                                        (extra-path   '()))
  "Add new execution node which installs PACKAGE-LIST."
  (let ((--type         (or type    'package))
        (--parents      (or parents '("install")))
        (--package-list package-list)
        (--src          src)
        (--name         name)
        (--lambda       nil)
        (--post-hook    post-hook)
        (--extra-path   extra-path))
    (setq --lambda
          (cond
           ((eq --type 'package) (lambda ()
                                   (mapcar #'serika-f/package/make-sure-installed
                                           --package-list)))
           ;; todo: add directory download
           ((eq --type 'download) (when (stringp --src)
                                    (lambda ()
                                      (let ((--destination (func/path-join serika-plugin-directory
                                                                           (url-unhex-string (file-name-nondirectory --src)))))
                                        (unless (file-exists-p --destination)
                                          (url-copy-file --src
                                                         --destination))))))
           ((eq --type 'git) (when (stringp --src)
                               (lambda ()
                                 (let ((--destination (func/path-join serika-plugin-directory
                                                                      (file-name-nondirectory --src))))
                                   (unless (file-exists-p --destination)
                                     (shell-command-to-string (format "git clone %s %s"
                                                                      --src
                                                                      --destination))
                                     (when --post-hook
                                       (shell-command-to-string (format "cd %s && %s"
                                                                        --destination
                                                                        --post-hook))))

                                   (when (file-accessible-directory-p --destination)
                                     (add-to-list 'load-path --destination)

                                     (cl-loop for --ep in --extra-path
                                              do
                                              (when (file-accessible-directory-p (func/path-join --destination
                                                                                                 --ep))
                                                (add-to-list 'load-path (func/path-join --destination --ep)))))))))
           (t nil)))

    (when --lambda
      (eg/add --serika-execution-graph
              :parents --parents
              :name    --name
              :func    --lambda))))



;; todo: merge these functions?
(defmacro serika-c/eg/add-many-by-name (name &rest args)
  "Add many execution nodes at once.
Example:
(serika-f/eg/add-many-by-name 'enode-name
                              (\"parents-1\")
                              lambda-1
                              (\"parents-2\")
                              lambda-2."
  `(when (cl-oddp (length ',args))
     (error "Length of ARGS must be even."))
  `(let ((--name ,name)
         (--args ',args))
     (cl-loop for --parents in --args       by #'cddr
              for --elem    in (cdr --args) by #'cddr
              do
              (eg/add --serika-execution-graph
                      :name    --name
                      :parents --parents
                      :func
(cond
 ((eq (car --elem)
      'function)
  (car (cdr --elem)))
 ((eq (car --elem)
      'lambda)
  (eval --elem))
 (t --elem))
))))

(defmacro serika-c/eg/add-many-by-parents (parents &rest args)
  "Add many execution nodes at once.
Example:
(serika-f/eg/add-many-by-parents parents
                                 'name-1
                                 lambda-1
                                 'name-2
                                 lambda-2."
  `(when (cl-oddp (length ',args))
     (error "Length of ARGS must be even."))
  `(let ((--parents ',parents)
         (--args    ',args))
     (cl-loop for --name in --args       by #'cddr
              for --elem in (cdr --args) by #'cddr
              do
              (eg/add --serika-execution-graph
                      :name    (car (cdr --name))
                      :parents --parents
                      :func
(cond
 ((eq (car --elem)
      'function)
  (car (cdr --elem)))
 ((eq (car --elem)
      'lambda)
  (eval --elem))
 (t --elem))
))))

(defun serika-c/eg/execute ()
  "Execute execution graph."
  (eg/execute --serika-execution-graph))

(provide 'core-execution)
;;; core-execution.el ends here
