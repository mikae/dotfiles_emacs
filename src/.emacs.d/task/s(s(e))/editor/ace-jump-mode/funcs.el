;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `ace-jump-mode'."
  (serika-c/eg/add-install :type 'git
                           :name 'ace-jump-mode
                           :src  "https://github.com/shinkiley/ace-jump-mode")

  (serika-c/eg/add-many-by-name 'ace-jump-mode
    ("require")
    (func/func/require 'ace-jump-mode)))
