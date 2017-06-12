;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika/text/evil ()
  "Configure `evil' for `text-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-set-initial-state 'text-mode 'normal))

(defun serika/text/interface ()
  "Configure interface for `text-mode'."
  (linum-mode  1))

(defun serika/text/buffer-local-variables ()
  "Configure buffer-local variables for `text-mode'."
  (setq tab-width 4))
