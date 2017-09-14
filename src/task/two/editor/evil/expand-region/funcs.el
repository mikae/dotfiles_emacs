;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `expand-region'."
  (serika-c/eg/add-install :type 'package
                           :name 'expand-region
                           :package-list '(expand-region)
                           :parents '("install"))

  (serika-c/eg/add-many-by-name 'expand-region
                                ("require")
                                (lambda ()
                                  (require 'expand-region))))
