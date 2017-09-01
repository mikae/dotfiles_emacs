;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure f."
  (serika-c/eg/add-install :type         'package
                           :name         'dash
                           :package-list '(dash)
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many-by-name 'dash
                                ("zero lib require")
                                (lambda ()
                                  (require 'dash)
                                  (dash-enable-font-lock))))
