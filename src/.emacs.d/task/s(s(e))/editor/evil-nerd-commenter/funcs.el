;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `evil-nerd-commenter'."
  (serika-c/eg/add-install :type 'git
                           :name 'ace-window
                           :src  "https://github.com/shinkiley/evil-nerd-commenter"
                           :parents '("install evil"))

  (serika-c/eg/add-many-by-name 'evil-nerd-commenter
    ("require evil")
    (func/func/require 'evil-nerd-commenter)))
