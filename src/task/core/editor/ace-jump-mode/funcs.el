;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/ace-jump-mode//require ()
  "Require modules for `ace-jump-mode'."
  (require 'ace-jump-mode))

(defun serika-g/ace-jump-mode//global-keymap ()
  "Configure evil to use `ace-jump-mode'."
  (global-set-key (kbd "A-c") 'ace-jump-word-mode)
  (global-set-key (kbd "A-C") 'ace-jump-char-mode)
  (global-set-key (kbd "A-v") 'ace-jump-line-mode))

;; Init
(defun init ()
  "Configure `ace-jump-mode'."
  (serika-c/eg/add-install :package-list '(ace-jump-mode)
                           :name 'ace-jump-mode)

  (serika-c/eg/add :parents '("require")
                   :name    'ace-jump-mode
                   :func    #'serika-g/ace-jump-mode//require)
  (serika-c/eg/add :parents '("global-keymap evil")
                   :name    'ace-jump-mode
                   :func    #'serika-g/ace-jump-mode//global-keymap)
  )
