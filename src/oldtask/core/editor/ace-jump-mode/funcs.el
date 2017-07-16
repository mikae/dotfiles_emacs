;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/ace-jump-mode//require ()
  "Require modules for `ace-jump-mode'."
  (require 'ace-jump-mode))

(defun serika-g/ace-jump-mode//evil ()
  "Configure evil to use `ace-jump-mode'."
  (define-key evil-normal-state-map (kbd "w") 'ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "L") 'ace-jump-line-mode))

;; Init
(defun init ()
  "Configure `ace-jump-mode'."
  (serika-g/ace-jump-mode//require)
  (serika-g/ace-jump-mode//evil))
