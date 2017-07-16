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
  (serika-g/magit//require)
  (serika-g/magit//global-keymap))
