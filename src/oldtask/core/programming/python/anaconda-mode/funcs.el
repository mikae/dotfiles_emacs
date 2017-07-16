;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/anaconda-mode//require ()
  "Require modules for `anaconda-mode'."
  (require 'anaconda-mode))

(defun serika-g/anaconda-mode//keymap ()
  "Configure `anaconda-mode-map'."
  (setq anaconda-mode-map (make-sparse-keymap)))

;; Init
(defun init ()
  "Configure `anaconda-mode-map'."
  (serika-g/anaconda-mode//require)
  (serika-g/anaconda-mode//keymap))
