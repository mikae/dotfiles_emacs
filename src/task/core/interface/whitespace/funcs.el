;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/whitespace/enable (&optional enable)
  "Enable `whitespace-mode'."
  (whitespace-mode (or enable +1)))

(defun serika-f/whitespace/toggle ()
  "Toggle `whitespace-mode'."
  (whitespace-mode (if whitespace-mode -1 +1)))

(defun init ()
  "Configure `whitespace'."
  (serika-c/eg/add-many-by-name 'whitespace
                                ("require")
                                (lambda ()
                                  (require 'whitespace))

                                ("settings")
                                (lambda ()
                                  )

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-x t w" #'serika-f/whitespace/toggle))))
