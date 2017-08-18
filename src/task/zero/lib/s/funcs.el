;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type         'package
                           :name         's
                           :package-list '(s)
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many-by-name 's
                        ("zero lib require")
                        (lambda ()
                          (require 's))))
