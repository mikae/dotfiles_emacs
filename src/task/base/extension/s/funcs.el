;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type         'package
                           :name         's
                           :package-list '(s)
                           :parents      '("base post install"))

  (serika-c/eg/add-many 's
                        ("base post require")
                        (lambda ()
                          (require 's))))
