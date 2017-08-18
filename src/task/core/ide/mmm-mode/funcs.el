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
  (serika-c/eg/add-install :package-list '(mmm-mode)
                           :name         'mmm-mode)

  (serika-c/eg/add-many-by-name 'mmm-mode
                        ("require")
                        (lambda ()
                          (require 'mmm-mode))))
