;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `buffer-move'."
  (serika-c/eg/add-install :package-list '(buffer-move)
                           :name 'buffer-move)

  (serika-c/eg/add-many-by-name 'buffer-move
                                ("require")
                                (lambda ()
                                  (require 'buffer-move))

                                ("settings")
                                (lambda ()
                                  (setq buffer-move-behavior 'swap)
                                  (setq buffer-move-stay-after-swap nil))))
