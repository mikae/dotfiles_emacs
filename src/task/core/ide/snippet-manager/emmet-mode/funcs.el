;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika/emmet/expand ()
  "Custom expand line. It ensures that `emmet-expand-line' will be invoked correctly in `evil-mode'."
  (interactive)
  (if (or evil-mode evil-local-mode)
      (cond ((evil-insert-state-p) (progn
                                     (call-interactively 'emmet-expand-line)))
            ((evil-normal-state-p) (save-excursion
                                     (evil-insert-state)
                                     (forward-char 1)
                                     (call-interactively 'emmet-expand-line)
                                     (evil-normal-state))))
    (call-interactively 'emmet-expand-line)))

(defun serika/emmet/activate ()
  "Activate `emmet-mode'."
  (emmet-mode +1))

;; Global
(defun serika/emmet//keymap ()
  "Configure `emmet-mode-keymap'."
  (defvar emmet-mode-keymap (make-sparse-keymap)))

(defun serika/emmet//settings ()
  "Configure `emmet-mode' variables."
  ;; Disable preview
  (setq emmet-preview-default nil))

;; Init
(defun init ()
  "Configure `emmet-mode'."
  (serika/emmet//keymap)
  (require 'emmet-mode)
  (serika/emmet//settings))
