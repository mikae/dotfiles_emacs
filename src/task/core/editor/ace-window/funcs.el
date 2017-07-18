;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/ace-window//require ()
  "Require modules."
  (require 'ace-window))

(defun serika-g/ace-window//settings ()
  "Configure `ace-window' settings."
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(defun serika-g/ace-window//global-keymap ()
  "Configure global keymap to use`ace-window'."
  (global-set-key (kbd "C-, w s") 'ace-window))

;; Init
(defun init ()
  "Configure ace-window."
  (serika-c/eg/add-install :package-list '(ace-window)
                           :name 'ace-window)

  (serika-c/eg/add :parents '("require")
                   :name    'ace-window
                   :func    #'serika-g/ace-window//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'ace-window
                   :func    #'serika-g/ace-window//settings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'ace-window
                   :func    #'serika-g/ace-window//settings)
  )
