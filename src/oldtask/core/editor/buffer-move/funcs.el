;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/buffer-move/require ()
  "Require modules for `buffer-move'."
  (require 'func-package)
  (require 'buffer-move))

(defun serika-g/buffer-move/settings ()
  "Configure `buffer-move' settings."
  (setq buffer-move-behavior 'swap)
  (setq buffer-move-stay-after-swap nil))


(defun serika-g/buffer-move/global-keymap ()
  "Configure `buffer-move' settings."
  (global-set-key (kbd "C-, w K") 'buf-move-up)
  (global-set-key (kbd "C-, w L") 'buf-move-right)
  (global-set-key (kbd "C-, w J") 'buf-move-down)
  (global-set-key (kbd "C-, w H") 'buf-move-left))

;; Init
(defun init ()
  "Configure `buffer-move'."
  (serika-g/buffer-move/settings)
  (serika-g/buffer-move/global-keymap))
