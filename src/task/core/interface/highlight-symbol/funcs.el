;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/highlight-symbol/activate ()
  "Activate `highlight-symbol' in current buffer."
  (highlight-symbol-mode +1))

;; Init
(defun init ()
  "Configure `highlight-symbol'."
  (serika-c/eg/add-install :type 'package
                           :name 'highlight-symbol
                           :package-list '(highlight-symbol))

  (serika-c/eg/add-many-by-name 'highlight-symbol
                        ("require")
                        (lambda ()
                          (require 'highlight-symbol))))
