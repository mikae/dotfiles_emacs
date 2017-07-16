;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/mmm-mode//activate ()
  "Activate `mmm-mode'."
  (mmm-mode +1))

;; Global
(defun serika-g/mmm-mode//require ()
  "Require modules for `mmm-mode'."
  (require 'mmm-mode))

(defun serika-g/mmm-mode//clear ()
  "Clear default `mmm-mode' state."
  nil)

;; Init
(defun init ()
  "Configure `mmm-mode'."
  (serika-g/mmm-mode//require)
  (serika-g/mmm-mode//clear))
