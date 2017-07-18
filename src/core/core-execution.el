;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'func-package)
(require 'func-execution)

(defvar --serika-execution-graph nil
  "Execution graph for all tasks.")

(defun serika-c/eg/create (&rest packages)
  (setq --serika-execution-graph (eg/create))

  (eg/create-path --serika-execution-graph "base require")
  (eg/create-path --serika-execution-graph "base configure")
  (eg/create-path --serika-execution-graph "base interface")

  (eg/create-path --serika-execution-graph "install")
  (eg/create-path --serika-execution-graph "require")
  (eg/create-path --serika-execution-graph "interface")
  (eg/create-path --serika-execution-graph "settings")
  (eg/create-path --serika-execution-graph "keymap")
  (eg/create-path --serika-execution-graph "global-keymap")
  (eg/create-path --serika-execution-graph "hook")
  (eg/create-path --serika-execution-graph "post"))

(defun serika-c/eg/event-nodes (&rest packages)
  )

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

(defun serika-c/eg/execute ()
  "Execute execution graph."
  (eg/execute --serika-execution-graph))

(provide 'core-execution)
;;; core-execution.el ends here
