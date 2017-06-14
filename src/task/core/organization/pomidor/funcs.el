;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'pomidor)

(defun serika/pomidor//settings ()
  "Configure `pomidor' settings."
  (setq pomidor-sound-tick nil)
  (setq pomidor-sound-tack nil)
  (setq pomidor-sound-overwork t)

  (setq pomidor-mode-map (make-sparse-keymap)))

(defun serika/pomidor//keymap ()
  "Configure `pomidor' keymap."
  (define-key pomidor-mode-map "n" 'pomidor-stop)
  (define-key pomidor-mode-map "b" 'pomidor-break)
  (define-key pomidor-mode-map "r" 'pomidor-reset)
  (define-key pomidor-mode-map "q" 'quit-window)
  (define-key pomidor-mode-map "s" 'pomidor-quit))

(defun serika/pomidor//global-keymap ()
  "Configure `pomidor' global keymap."
  (global-set-key (kbd "<C-m> p") 'pomidor))

(defun init ()
  "Configure `pomidor'."
  (serika/pomidor//settings)
  (serika/pomidor//keymap)
  (serika/pomidor//global-keymap))
