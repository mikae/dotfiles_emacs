;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ace-jump-mode)

(defun serika/ace-jump-mode//evil ()
  "Configure evil to use `ace-jump-mode'."
  (define-key evil-normal-state-map (kbd "w") 'ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "L") 'ace-jump-line-mode))

(defun init ()
  "Configure `ace-jump-mode'."
  (serika/ace-jump-mode//evil))
