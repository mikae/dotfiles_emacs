;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type         'package
                           :name         'f
                           :package-list '(f)
                           :parents      '("base post install"))

  (serika-c/eg/add-many 'f
                        ("base post require")
                        (lambda ()
                          (require 'f))))
