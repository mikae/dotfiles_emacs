;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'vimperator-mode)

;; Global
(defun serika/vimperator//auto-mode-alist ()
  "Configure `auto-mode-alist' for `vimperator'."
  (add-to-list 'auto-mode-alist '("\\.vimperatorrc\\'" . vimperator-mode)))

;; Local
(defun serika/vimperator//evil ()
  "Configure `auto-mode-alist' for `vimperator'."
  (setq evil-shift-width 2)
  (evil-local-mode)
  (evil-normal-state))

(defun serika/vimperator//buffer-local-variables ()
  "Configure buffer-local variables for `vimperator'."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika/vimperator//interface ()
  "Configure interface for `emacs-lisp' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun init ()
  "Configure `vimperator'."
  (serika/vimperator//auto-mode-alist)

  (add-hook 'vimperator-mode-hook #'serika/vimperator//evil)
  (add-hook 'vimperator-mode-hook #'serika/vimperator//buffer-local-variables)
  (add-hook 'vimperator-mode-hook #'serika/vimperator//interface))
