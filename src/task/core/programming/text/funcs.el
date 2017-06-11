;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika/text/evil ()
  "Configure `evil' for `text-mode'."
  (evil-set-initial-state 'text-mode 'normal))

(defun serika/text/interface ()
  "Configure interface for `text-mode'."
  (linum-mode  1))

(defun serika/text/buffer-local-variables ()
  "Configure buffer-local variables for `text-mode'."
  (setq tab-width 4)
  (setq evil-shift-width 4))
