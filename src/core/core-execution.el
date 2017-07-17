;;; package --- Summary
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
  (eg/create-path --serika-execution-graph "require")
  (eg/create-path --serika-execution-graph "global-keymap")
  )

(defun serika-c/eg/packages (&rest packages)
  "Install packages."
  (dolist (package (packages))
    (serika-f/package/make-sure-installed package-name)))

(cl-defun serika-c/eg/add (&key name (last '__unnamed__)
                                &key parents (last nil)
                                &key func (last nil)
                                &key node (last nil))
  (eg/add --serika-execution-graph
          :name    name
          :parents parents
          :func    func
          :node    node))

(defun serika-c/eg/execute ()
  "Execute execution graph."
  (eg/execute --serika-execution-graph))

(provide 'core-execution)
;;; core-execution.el ends here
