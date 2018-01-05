;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `evil-visualstar'."
  (serika-c/eg/add-install :type 'git
                           :name 'evil-visualstar
                           :src  "https://github.com/shinkiley/evil-visualstar")

  (serika-c/eg/add-many-by-name 'evil-visualstar
    ("require")
    (func/func/require 'evil-visualstar)))
