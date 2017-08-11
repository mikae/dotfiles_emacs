;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `magit'."
  (serika-c/eg/add-install :type         'package
                           :package-list '(magit)
                           :name         'magit)

  (serika-c/eg/add-many 'magit
                        ("require")
                        (lambda ()
                          (require 'magit))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "C-x m i" 'magit-init
                                                     "C-x m s" 'magit-status))))
