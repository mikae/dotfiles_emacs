;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `expand-region'."
  (serika-c/eg/add-install :type 'git
                           :name 'expand-region
                           :src "https://github.com/shinkiley/expand-region.el")

  (serika-c/eg/add-many-by-name 'expand-region
    ("require")
    (func/func/require 'expand-region)))
