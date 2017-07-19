;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/vimperator//require ()
  "Require modules for `vimperator'."
  (require 'vimperator-mode))

(defun serika-g/vimperator//settings ()
  "Configure `vimperator'."
  (add-to-list 'auto-mode-alist '("\\.vimperatorrc\\'" . vimperator-mode))
  (add-to-list 'auto-mode-alist '("\\.vimperatorrc\\.after\\'" . vimperator-mode))
  (add-to-list 'auto-mode-alist '("\\.vimp\\'" . vimperator-mode))
  (add-to-list 'auto-mode-alist '("\\.vimp\\.after\\'" . vimperator-mode)))

;; Local
(defun serika-l/vimperator//evil ()
  "Configure `auto-mode-alist' for `vimperator'."
  (setq evil-shift-width 2)
  (evil-local-mode)
  (evil-normal-state))

(defun serika-l/vimperator//buffer-local-variables ()
  "Configure buffer-local variables for `vimperator'."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/vimperator//interface ()
  "Configure interface for `emacs-lisp' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun init ()
  "Configure `vimperator'."
  (serika-c/eg/add :parents '("require")
                   :name    'vimperator
                   :func    #'serika-g/vimperator//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'vimperator
                   :func    #'serika-g/vimperator//settings)

  (serika-c/eg/add :parents '("hook")
                   :name    'vimperator
                   :func    (lambda ()
                              (add-hook 'vimperator-mode-hook #'serika-l/vimperator//evil)
                              (add-hook 'vimperator-mode-hook #'serika-l/vimperator//buffer-local-variables)
                              (add-hook 'vimperator-mode-hook #'serika-l/vimperator//interface))))
