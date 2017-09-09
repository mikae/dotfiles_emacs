;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `visual-regexp'."
  (serika-c/eg/add-install :type 'package
                           :name 'visual-regexp
                           :package-list '(visual-regexp
                                           visual-regexp-steroids)
                           :parents '("install"))

  (serika-c/eg/add-many-by-name 'visual-regexp
                                ("require")
                                (lambda ()
                                  (require 'visual-regexp)
                                  (require 'visual-regexp-steroids))))
