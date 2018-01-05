;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure f."
  (serika-c/eg/add-install :type    'git
                           :name    'dash
                           :src     "https://github.com/shinkiley/dash.el"
                           :parents '("zero lib install"))

  (serika-c/eg/add-many-by-name 'dash
    ("zero lib require")
    (progn
      (require 'dash)
      (dash-enable-font-lock))))
