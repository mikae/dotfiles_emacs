;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/web-beautify//require ()
  "Require modules for `web-beautify'."
  (setq web-beautify-indent-level 2)
  (require 'web-beautify))

(defun init ()
  "Configure `web-beautify'."
  (serika-g/web-beautify//require))
