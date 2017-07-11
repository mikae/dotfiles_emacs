;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/linum//require ()
  "Require modules for linum"
  (require 'linum))

;; Init
(defun init ()
  "Configure `linum'."
  (serika/linum//require))
