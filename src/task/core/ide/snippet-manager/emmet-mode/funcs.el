;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/emmet-mode//keymap ()
  "Configure `emmet-mode-keymap'."
  (defvar emmet-mode-keymap (make-sparse-keymap))
  (define-key emmet-mode-keymap (kbd "<C-tab>") 'emmet-expand-line))

(defun serika/emmet-mode//settings ()
  "Configure `emmet-mode' variables."
  ;; Disable preview
  (setq emmet-preview-default nil))

(defun init ()
  "Configure `emmet-mode'."
  (serika/emmet-mode//keymap)
  (require 'emmet-mode)
  (serika/emmet-mode//settings))
