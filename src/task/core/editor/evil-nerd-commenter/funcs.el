;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/evil-nerd-commenter//require ()
  "Require modules for `evil-nerd-commenter'."
  (require 'evil-nerd-commenter))

;; Init
(defun init ()
  "Configure `evil-nerd-commenter'."
  (serika-c/eg/add-install :package-list '(evil-nerd-commenter)
                           :name 'evil-nc
                           :parents '("install evil"))

  (serika-c/eg/add :parents '("require evil")
                   :name    'evil-nc
                   :func    #'serika-g/evil-nerd-commenter//require)
  )
