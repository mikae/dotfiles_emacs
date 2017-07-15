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
  (serika-g/linum//require))
