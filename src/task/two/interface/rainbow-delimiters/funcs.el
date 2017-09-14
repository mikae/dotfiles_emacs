;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/rainbow-delimiters/activate ()
  "Activate `rainbow-delimiters' in current buffer."
  (rainbow-delimiters-mode +1))

;; Init
(defun init ()
  "Configure `rainbow-delimiters'."
  (serika-c/eg/add-install :package-list '(rainbow-delimiters)
                           :name         'rainbow-delimiters)

  (serika-c/eg/add-many-by-name 'rainbow-delimiters
                        ("require")
                        (lambda ()
                          (require 'rainbow-delimiters))))
