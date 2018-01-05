;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `help-fns+'."
  (serika-c/eg/add-install :type    'git
                           :name    's
                           :src     "https://github.com/shinkiley/s.el"
                           :parents '("zero lib install"))

  (serika-c/eg/add-many-by-name 's
    ("zero lib require")
    (func/func/require 's)))
