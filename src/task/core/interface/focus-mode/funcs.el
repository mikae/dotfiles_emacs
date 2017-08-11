;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/focus/activate ()
  "Activate `focus-mode' in current buffer."
  (focus-mode +1))

;; Init
(defun init ()
  "Configure `focus-mode'."
  (serika-c/eg/add-install :type 'package
                           :name 'focus-mode
                           :package-list '(focus))
  (serika-c/eg/add-many 'focus-mode
                        ("require")
                        (lambda ()
                          (require 'focus))))

