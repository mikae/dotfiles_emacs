;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `ace-jump-mode'."
  (serika-c/eg/add-install :type 'package
                           :name 'ace-jump-mode
                           :package-list '(ace-jump-mode)
                           :parents '("install"))

  (serika-c/eg/add-many-by-name 'ace-jump-mode
                                ("require")
                                (lambda ()
                                  (require 'ace-jump-mode))))
