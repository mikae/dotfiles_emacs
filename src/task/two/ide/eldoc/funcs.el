;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/eldoc/activate ()
  "Activate `eldoc' in curreng buffer"
  (eldoc-mode +1))


;; Global
(defun init ()
  "Configure `eldoc'."
  (serika-c/eg/add-many-by-name 'eldoc
                        ("require")
                        (lambda ()
                          (require 'eldoc))))
