;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/sh//keymap ()
  "Configure `sh-mode-map'."
  (setq sh-mode-map (make-sparse-keymap)))

(defun serika/sh//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode)))

;; Local
(defun serika/sh//evil ()
  "Configure evil for `sh-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/sh//local-keymap ()
  "Configure local keymap"
  )

(defun init ()
  "Configure `sh-mode'."
  (serika/sh//keymap)

  (add-hook 'sh-mode-hook 'serika/sh//evil))
