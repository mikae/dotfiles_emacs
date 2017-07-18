;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/multi-compile//require ()
  "Require modules for `multi-compile'."
  (require 'multi-compile))

(defun serika-g/multi-compile//settings ()
  "Configure variables."
  (setq multi-compile-alist ())
  (setq multi-compile-completion-system 'helm))

;; Init
(defun init ()
  "Configure `multi-compile'."
  (serika-c/eg/add-install :package-list '(multi-compile)
                           :name         'multi-compile)

  (serika-c/eg/add :parents '("require")
                   :name    'multi-compile
                   :func    #'serika-g/multi-compile//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'multi-compile
                   :func    #'serika-g/multi-compile//settings))
