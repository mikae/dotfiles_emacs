;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/aggressive-indent/activate ()
  "Activate `aggressive-indent' in current buffer."
  (aggressive-indent-mode +1))

(defun serika-f/aggressive-indent/toggle ()
  "Toggle agressive indent."
  (interactive)
  (aggressive-indent-mode (if aggressive-indent-mode
                              -1
                            +1)))

;; Init
(defun init ()
  "Configure `aggressive-indent'."
  (serika-c/eg/add-install :type 'package
                           :name 'aggressive-indent
                           :package-list '(aggressive-indent))

  (serika-c/eg/add-many-by-name 'aggressive-indent
                                ("require")
                                (lambda ()
                                  (require 'aggressive-indent))

                                ("settings")
                                (lambda ()
                                  (setq aggressive-indent-dont-indent-if ()))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-x t a" #'serika-f/aggressive-indent/toggle))))
