;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/linum//require ()
  "Require modules for linum"
  (require 'linum))

;; Init
(defun init ()
  "Configure `linum'."
  (serika-c/eg/add :parents '("require")
                   :name    'linum
                   :func    #'serika-g/linum//require))
