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
  (serika-c/eg/add-install :type    'git
                           :name    'rainbow-mode
                           :src     "https://github.com/shinkiley/rainbow-mode")

  (serika-c/eg/add-many-by-name 'rainbow-mode
    ("require")
    (func/func/require 'rainbow-mode)))
