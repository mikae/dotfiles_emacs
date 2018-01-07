;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure f."
  (serika-c/eg/add-install :type         'git
                           :name         'f
                           :src          "https://github.com/shinkiley/f.el"
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many-by-name 'f
    ("zero lib require")
    (func/func/require 'f)))
