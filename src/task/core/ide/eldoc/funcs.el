;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/eldoc/activate ()
  "Activate `eldoc'."
  (turn-on-eldoc-mode))

;; Global
(defun serika-gc/eldoc//require ()
  "Require modules for `eldoc'."
  (require 'eldoc))

(defun init ()
  "Configure `eldoc'."
  (serika-c/eg/add :parents '("require")
                   :name    'eldoc
                   :func    #'serika-gc/eldoc//require))