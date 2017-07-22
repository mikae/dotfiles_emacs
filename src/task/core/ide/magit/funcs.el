;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/magit//require ()
  "Require modules for `magit'."
  (require 'magit))

(defun serika-g/magit//global-keymap ()
  "Configure global keymap."
  (global-set-key (kbd "C-x m i") 'magit-init)
  (global-set-key (kbd "C-x m s") 'magit-status))

;; Init
(defun init ()
  "Configure `magit'."
  (serika-c/eg/add-install :package-list '(magit)
                           :name         'magit)

  (serika-c/eg/add :parents '("require")
                   :name    'magit
                   :func    #'serika-g/magit//require)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'magit
                   :func    #'serika-g/magit//global-keymap))
