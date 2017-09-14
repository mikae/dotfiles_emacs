;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/emmet/expand ()
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

(defun serika-f/emmet/activate ()
  "Activate `emmet-mode'."
  (emmet-mode +1))

;; Init
(defun init ()
  "Configure `emmet-mode'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(emmet-mode)
                           :name         'emmet-mode)

  (serika-c/eg/add-many-by-name 'emmet-mode
                        ("require")
                        (lambda ()
                          (defvar emmet-mode-keymap (make-sparse-keymap))
                          ;; Because `emmet' uses keymap as local variable
                          (require 'emmet-mode) )

                        ("settings")
                        (lambda ()
                          (setq emmet-preview-default nil))))
