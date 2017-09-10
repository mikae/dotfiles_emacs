;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `makefile' buffers."
  (serika-c/eg/add-many-by-name 'makefile
                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'makefile-mode
                                                                 "Makefile$"))))
