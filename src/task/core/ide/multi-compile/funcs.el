;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/multi-compile/run ()
  "Saves current buffer and invokes `multi-compile-run'."
  (interactive)
  (save-buffer)
  (multi-compile-run))

;; Init
(defun init ()
  "Configure `multi-compile'."
  (serika-c/eg/add-install :package-list '(multi-compile)
                           :name         'multi-compile)
  (serika-c/eg/add-many-by-name 'multi-compile
                                ("require")
                                (lambda ()
                                  (require 'multi-compile))

                                ("settings")
                                (lambda ()
                                  (setq multi-compile-alist ())
                                  (setq multi-compile-completion-system 'helm))))
