;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'mmm-mode)

(defun serika/mmm-mode/add (mode submode begin-pattern end-pattern)
  "Add new submode to `mmm-mode'."
  (lexical-let ((mode          mode)
                (submode       submode)
                (begin-pattern begin-pattern)
                (end-pattern   end-pattern))
    (mmm-add-classes
     `((,mode
        ,submode
        ,begin-pattern
        ,end-pattern)))))

(defun init ()
  "Configure `mmm-mode'."
  (setq mmm-global-mode 'maybe))
