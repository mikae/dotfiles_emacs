;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type 'download
                           :name 'help-fns+
                           :src  "https://raw.githubusercontent.com/shinkiley/emacswiki.org/master/help-fns%2B.el")

  (serika-c/eg/add-many-by-name 'help-fns+
    ("require")
    (func/func/require 'help-fns+)

    ("global-keymap")
    (func/keymap/define-global "C-x h K" 'describe-keymap)))
