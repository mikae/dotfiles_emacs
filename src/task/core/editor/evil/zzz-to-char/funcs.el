;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `zzz-to-char'."
  (serika-c/eg/add-install :type 'package
                           :name 'zzz-to-char
                           :package-list '(zzz-to-char)
                           :parents '("install"))

  (serika-c/eg/add-many-by-name 'zzz-to-char
                                ("require")
                                (lambda ()
                                  (require 'zzz-to-char))))
