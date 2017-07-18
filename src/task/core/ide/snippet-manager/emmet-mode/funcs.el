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

;; Global
(defun serika-g/emmet//keymap ()
  "Configure `emmet-mode-keymap'."
  (defvar emmet-mode-keymap (make-sparse-keymap))
  ;; Because `emmet' uses keymap as local variable
  (require 'emmet-mode))

(defun serika-g/emmet//settings ()
  "Configure `emmet-mode' variables."
  (setq emmet-preview-default nil))

;; Init
(defun init ()
  "Configure `emmet-mode'."
  (serika-c/eg/add-install :package-list '(emmet-mode)
                           :name         'emmet-mode)

  (serika-c/eg/add :parents '("keymap")
                   :name    'emmet-mode
                   :func    #'serika-g/emmet//keymap)

  (serika-c/eg/add :parents '("settings")
                   :name    'emmet-mode
                   :func    #'serika-g/emmet//settings))
