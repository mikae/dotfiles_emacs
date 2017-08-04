;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type 'download
                           :name 'help-fns+
                           :src  "https://raw.githubusercontent.com/mikae/emacswiki.org/master/help-fns%2B.el")

  (serika-c/eg/add-many 'help-fns+
                        ("require")
                        (lambda ()
                          (require 'help-fns+))

                        ("global-keymap")
                        (lambda ()
                          (global-set-key (kbd "C-x h K") 'describe-keymap))))
