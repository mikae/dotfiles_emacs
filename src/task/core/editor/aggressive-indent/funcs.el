;;; package --- Summary
;;; Commentary: 
;;; Code:

(defun serika-f/aggressive-indent/activate ()
  "Activate `aggressive-indent' in current buffer."
  (aggressive-indent-mode +1))

;; Init
(defun init ()
  "Configure `aggressive-indent'."
  (serika-c/eg/add-install :type 'package
                           :name 'aggressive-indent
                           :package-list '(aggressive-indent))
  (serika-c/eg/add-many 'aggressive-indent
                        ("require")
                        (lambda ()
                          (require 'aggressive-indent))))
