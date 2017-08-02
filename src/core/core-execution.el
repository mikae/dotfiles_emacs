;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'func-package)
(require 'func-execution)

(defvar --serika-execution-graph nil
  "Execution graph for all tasks.")

(defun serika-c/eg/create (&rest packages)
  (setq --serika-execution-graph (eg/create))

	(dolist (task '(
									;; base tasks
									"base require"
									"base configure"
									"base interface"

									;; tasks
									"install"
									"require"
									"interface"
									"settings"
									"keymap"
									"global-keymap"
									"hook"
									"post activate"))
		(eg/create-path --serika-execution-graph task)))

(defun serika-c/eg/packages (&rest packages)
  "Install packages."
  (dolist (package (packages))
    (serika-f/package/make-sure-installed package-name)))

(cl-defun serika-c/eg/add (&key name         (last '__unnamed__)
                                &key parents (last nil)
                                &key func    (last nil)
                                &key node    (last nil))
  "Add new execution node."
  (eg/add --serika-execution-graph
          :name    name
          :parents parents
          :func    func
          :node    node))

(cl-defun serika-c/eg/add-install (&key package-list (last '())
                                        &key name    (last '__unnamed__)
                                        &key parents (last nil))
  "Add new execution node which installs PACKAGE-LIST."
  (let ((package-list package-list))
    (eg/add --serika-execution-graph
            :name    name
            :parents (or parents '("install"))
            :func    (lambda ()
                       (mapcar #'serika-f/package/make-sure-installed
                               package-list)))))

(defmacro serika-c/eg/add-many (name &rest args)
  "Add many execution nodes at once.
Example: ."
  `(let ((--length (length ',args))
         (--args   ',args)
         (--name   ,name))
     (when (cl-evenp --length)
       (while --args
         (eg/add --serika-execution-graph
                 :name    --name
                 :parents (car --args)
                 :func    (let* ((--elem (car (cdr --args)))
                                 (--car  (car --elem)))
                            (if (eq --car
                                    'function)
                                (car (cdr --elem))
                              --elem)))
         (setq --args
               (nthcdr 2 --args))))))

(defun serika-c/eg/execute ()
  "Execute execution graph."
  (eg/execute --serika-execution-graph))

(provide 'core-execution)
;;; core-execution.el ends here
