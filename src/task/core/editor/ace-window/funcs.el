;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ace-window)

(defun serika/ace-window//settings ()
  "Configure `ace-window' settings."
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(defun serika/ace-window//global-keymap ()
  "Configure global keymap to use`ace-window'."
  (global-set-key (kbd "C-, w s") 'ace-window))

(defun init ()
  "Configure ace-window."
  (serika/ace-window//settings)
  (serika/ace-window//global-keymap))
