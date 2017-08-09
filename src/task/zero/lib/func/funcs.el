;;; package --- Summary
;;; Commentary: 
;;; Code:

(defun init ()
  "Configure `func'."
  (serika-c/eg/add-install :type         'git
                           :name         'func
                           :src          "https://github.com/mikae/elisp-func"
                           :parents      '("zero lib install"))

  (serika-c/eg/add-many 'func
                        ("zero lib require")
                        (lambda ()
                          (require 'func-buffer)
                          (require 'func-char)
                          (require 'func-eval)
                          (require 'func-file)
                          (require 'func-func)
                          (require 'func-hook)
                          (require 'func-keymap)
                          (require 'func-list)
                          (require 'func-logging)
                          (require 'func-path)
                          (require 'func-string)
                          (require 'func-symbol)
                          (require 'func-system)
                          (require 'func-tramp)
                          (require 'func-window))))
