;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `evil-nerd-commenter'."
  (serika-c/eg/add-install :type 'package
                           :name 'evil-nerd-commenter
                           :package-list '(evil-nerd-commenter)
                           :parents '("install evil"))

  (serika-c/eg/add-many-by-name 'evil-nerd-commenter
                                ("require evil")
                                ;; (func/func/requirer 'evil-nerd-commenter)
                                (lambda ()
                                  (require 'evil-nerd-commenter))
                                )
  )
