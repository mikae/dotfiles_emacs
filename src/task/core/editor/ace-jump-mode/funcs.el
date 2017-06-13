;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

(require 'ace-jump-mode)

(defun serika/ace-jump-mode/configure ()
  "Configure `ace-jump-mode'."
  (define-key evil-normal-state-map (kbd "w") 'ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "L") 'ace-jump-line-mode))
