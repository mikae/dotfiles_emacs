;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/undo-tree//activate ()
  "Activate `undo-tree-mode' global mode."
  (global-undo-tree-mode +1))

;; Init
(defun init ()
  "Configure `undo-tree'."
  (serika-c/eg/add-install :type 'git
                           :name 'undo-tree
                           :src  "https://github.com/shinkiley/undo-tree")

  (serika-c/eg/add-many-by-name 'undo-tree
    ("require")
    (func/func/require 'undo-tree)

    ("post activate")
    (serika-f/undo-tree//activate)))
