;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/web-beautify//require ()
  "Require modules for `web-beautify'."
  (setq web-beautify-indent-level 2)
  (require 'web-beautify))

(defun init ()
  "Configure `web-beautify'."
  (serika-c/eg/add :parents '("require")
                   :name    'web-beautify
                   :func    #'serika-g/web-beautify//require))
