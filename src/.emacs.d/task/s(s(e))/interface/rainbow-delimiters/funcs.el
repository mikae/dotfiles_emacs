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
  (serika-c/eg/add-install :type    'git
                           :name    'rainbow-delimiters
                           :src     "https://github.com/shinkiley/rainbow-delimiters")

  (serika-c/eg/add-many-by-name 'rainbow-delimiters
    ("require")
    (func/func/require 'rainbow-delimiters)))
