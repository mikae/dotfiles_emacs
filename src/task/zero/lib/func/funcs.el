;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `func'."
  (serika-c/eg/add-install :type         'git
                           :name         'func
                           :src          "https://github.com/mikae/elisp-func"
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many-by-name 'func
                                ("zero lib require")
                                (lambda ()
                                  (require 'func-all))))
