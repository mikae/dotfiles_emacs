;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `visual-regexp'."
  (dolist (--package '((visual-regexp          . "https://github.com/shinkiley/visual-regexp.el")
                       (visual-regexp-steroids . "https://github.com/shinkiley/visual-regexp-steroids.el")))
    (serika-c/eg/add-install :type 'git
                             :name (car --package)
                             :src  (cdr --package)))

  (serika-c/eg/add-many-by-name 'visual-regexp
    ("require")
    (func/func/require 'visual-regexp
                       'visual-regexp-steroids)))
