;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/undo-tree//activate ()
  "Activate `undo-tree-mode' global mode."
  (global-undo-tree-mode +1))

;; Init
(defun init ()
  "Configure `undo-tree'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(undo-tree)
                           :name 'undo-tree)
  (serika-c/eg/add-many-by-name 'undo-tree
                        ("require")
                        (lambda ()
                          (require 'undo-tree))

                        ("post activate")
                        (lambda ()
                          (global-undo-tree-mode +1))))
