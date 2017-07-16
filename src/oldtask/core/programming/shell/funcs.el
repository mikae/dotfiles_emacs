;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/sh//keymap ()
  "Configure `sh-mode-map'."
  (setq sh-mode-map (make-sparse-keymap)))

(defun serika-g/sh//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode)))

;; Local
(defun serika-l/sh//evil ()
  "Configure evil for `sh-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

(defun init ()
  "Configure `sh-mode'."
  (serika-g/sh//keymap)
  (serika-g/sh//auto-mode-alist)

  (add-hook 'sh-mode-hook 'serika-l/sh//evil))
