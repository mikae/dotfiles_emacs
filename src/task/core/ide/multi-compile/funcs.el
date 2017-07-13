;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/multi-compile//require ()
  "Require modules for `multi-compile'."
  (require 'multi-compile))

(defun serika/multi-compile//variables ()
  "Configure variables."
  (setq multi-compile-alist ())
  (setq multi-compile-completion-system 'helm))

(defun init ()
  "Configure `multi-compile'."
  (serika/multi-compile//require)
  (serika/multi-compile//variables))
