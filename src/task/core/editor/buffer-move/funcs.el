;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `buffer-move'."
  (serika-c/eg/add-install :package-list '(buffer-move)
                           :name 'buffer-move)

  (serika-c/eg/add-many 'buffer-move
                        ("require")
                        (lambda ()
                          (require 'buffer-move))

                        ("settings")
                        (lambda ()
                          (setq buffer-move-behavior 'swap)
                          (setq buffer-move-stay-after-swap nil))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global
                           "C-w N" 'buf-move-left
                           "C-w E" 'buf-move-down
                           "C-w I" 'buf-move-up
                           "C-w O" 'buf-move-right))))
