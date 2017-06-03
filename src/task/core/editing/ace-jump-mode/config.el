;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)

(serika/package/make-sure-installed 'ace-jump-mode)

(require 'ace-jump-mode)

(define-key evil-normal-state-map (kbd "w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "L") 'ace-jump-line-mode)
