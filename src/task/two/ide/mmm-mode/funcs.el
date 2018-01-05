;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/mmm-mode//activate ()
  "Activate `mmm-mode'."
  (mmm-mode +1))

;; Init
(defun init ()
  "Configure `mmm-mode'."
  (serika-c/eg/add-install :type    'git
                           :name    'mmm-mode
                           :src     "https://github.com/shinkiley/mmm-mode")

  (serika-c/eg/add-many-by-name 'mmm-mode
    ("require")
    (func/func/require 'mmm-mode)))
