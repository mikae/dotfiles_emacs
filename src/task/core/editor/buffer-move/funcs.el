;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

(require 'buffer-move)

(defun serika/buffer-move/settings ()
  "Configure `buffer-move' settings."
  (setq buffer-move-behavior 'swap)
  (setq buffer-move-stay-after-swap nil))


(defun serika/buffer-move/global-keymap ()
  "Configure `buffer-move' settings."
  (global-set-key (kbd "C-, w K") 'buf-move-up)
  (global-set-key (kbd "C-, w L") 'buf-move-right)
  (global-set-key (kbd "C-, w J") 'buf-move-down)
  (global-set-key (kbd "C-, w H") 'buf-move-left))
