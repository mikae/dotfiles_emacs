;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/tern/activate ()
  "Activate tern in current buffer."
  (tern-mode +1))

;; Init
(defun init ()
  "Configure tern."
  (serika-c/eg/add-install :type       'git
                           :name       'tern
                           :src        "https://github.com/mikae/tern"
                           :extra-path '("emacs")
                           :post-hook  "npm install"
                           :parents    '("install js2"))

  (serika-c/eg/add-many-by-name 'tern
                                ("require js2")
                                ;; (func/func/requirer 'tern)
                                (lambda ()
                                  (require 'tern))

                                ("keymap js2")
                                (lambda ()
                                  (func/keymap/save   tern-mode-keymap)
                                  (func/keymap/create tern-mode-keymap))))