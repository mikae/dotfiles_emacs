;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'help-fns+)

(defun serika/help-fns+//global-keymap ()
  "Configure global keymap for using of `help-fns+' functions."
  (global-set-key (kbd "C-h K") 'describe-keymap))
