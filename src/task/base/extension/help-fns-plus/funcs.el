;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/help-fns+//require ()
  "Require modules for `help-fns+'."
  (require 'help-fns+))

(defun serika-g/help-fns+//global-keymap ()
  "Configure global keymap for using of `help-fns+' functions."
  (global-set-key (kbd "C-h K") 'describe-keymap))

(defun init ()
  "Configure `help-fns+'."
  (serika-g/help-fns+//require)
  (serika-g/help-fns+//global-keymap))
