;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/rainbow-delimiters//require ()
  "Require modules for `rainbow-delimiters'."
  (require 'rainbow-delimiters))

;; Init
(defun init ()
  "Configure `rainbow-delimiters'."
  (serika-g/rainbow-delimiters//require))
