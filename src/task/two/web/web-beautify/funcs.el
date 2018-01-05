;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `web-beautify'."
  (serika-c/eg/add-install :type 'git
                           :name 'web-beautify
                           :src  "https://github.com/shinkiley/web-beautify")

  (serika-c/eg/add-many-by-name 'web-beautify
    ("require")
    (func/func/require 'web-beautify)

    ("settings")
    (setq web-beautify-indent-level 2)))
