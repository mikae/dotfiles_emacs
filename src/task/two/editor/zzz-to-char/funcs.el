;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `zzz-to-char'."
  (serika-c/eg/add-install :type 'git
                           :name 'zzz-to-char
                           :src "https://github.com/shinkiley/zzz-to-char")

  (serika-c/eg/add-many-by-name 'zzz-to-char
    ("require")
    (func/func/require 'zzz-to-char)))
