;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/multi-compile//require ()
  "Require modules for `multi-compile'."
  (require 'multi-compile))

(defun serika-g/multi-compile//variables ()
  "Configure variables."
  (setq multi-compile-alist ())
  (setq multi-compile-completion-system 'helm))

;; Init
(defun init ()
  "Configure `multi-compile'."
  (serika-g/multi-compile//require)
  (serika-g/multi-compile//variables))
