;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'pomidor)

(setq pomidor-sound-tick nil)
(setq pomidor-sound-tack nil)
(setq pomidor-sound-overwork t)

(setq pomidor-mode-map (make-sparse-keymap))

(define-key pomidor-mode-map "n" 'pomidor-stop)
(define-key pomidor-mode-map "b" 'pomidor-break)
(define-key pomidor-mode-map "r" 'pomidor-reset)
(define-key pomidor-mode-map "q" 'quit-window)
(define-key pomidor-mode-map "s" 'pomidor-quit)

(global-set-key (kbd "<C-m> p") 'pomidor)
