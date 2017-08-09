;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure f."
  (serika-c/eg/add-install :type         'package
                           :name         'f
                           :package-list '(f)
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many 'f
                        ("zero lib require")
                        (lambda ()
                          (require 'f))))
