;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/help-fns+//require ()
  "Require modules for `help-fns+'."
  (require 'help-fns+))

(defun serika-g/help-fns+//global-keymap ()
  "Configure global keymap for using of `help-fns+' functions."
  (global-set-key (kbd "C-x h K") 'describe-keymap))

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add :parents '("require")
                   :name    'help-fns+
                   :func    #'serika-g/help-fns+//require)
  (serika-c/eg/add :parents '("global-keymap")
                   :name    'help-fns+
                   :func    #'serika-g/help-fns+//global-keymap)
  )
