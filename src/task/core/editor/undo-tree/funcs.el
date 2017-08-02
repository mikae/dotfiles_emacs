;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-gf/undo-tree//activate ()
  "Activate `undo-tree-mode' global mode."
  (global-undo-tree-mode +1))

;; Global
(defun serika-gc/undo-tree//require ()
  "Require modules for `undo-tree'."
  (require 'undo-tree))

;; Init
(defun init ()
  "Configure `undo-tree'."
  (serika-c/eg/add-install :package-list '(undo-tree)
                           :name 'undo-tree)

  (serika-c/eg/add :parents '("require")
                   :name    'undo-tree
                   :func    #'serika-gc/undo-tree//require)

  (serika-c/eg/add :parents '("post activate")
                   :name    'undo-tree
                   :func    #'serika-gf/undo-tree//activate))
