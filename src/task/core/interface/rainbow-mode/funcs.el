;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/rainbow-mode/activate ()
  "Activate rainbow mode in current buffer."
  (rainbow-mode +1))

;; Init
(defun init ()
  "Configure `rainbow-mode'."
  (serika-c/eg/add-install :package-list '(rainbow-mode)
                           :name         'rainbow-mode)

  (serika-c/eg/add-many-by-name 'rainbow-mode
                        ("require")
                        (lambda ()
                          (require 'rainbow-mode))))
