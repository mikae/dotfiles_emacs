;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)

;; Install package
(serika/package/make-sure-installed 'buffer-move)

;; Require
(require 'buffer-move)

;; Configure variables
(setq buffer-move-behavior 'swap)
(setq buffer-move-stay-after-swap nil)

;; Configure bindings
(global-set-key (kbd "C-, w K") 'buf-move-up)
(global-set-key (kbd "C-, w L") 'buf-move-right)
(global-set-key (kbd "C-, w J") 'buf-move-down)
(global-set-key (kbd "C-, w H") 'buf-move-left)
