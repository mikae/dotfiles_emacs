;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'anaconda-mode)
(require 'company-anaconda)
(require 'pyenv-mode)

;; Global
(defun serika/python//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

;; Local
(defun serika/python//evil ()
  "Configure `evil' for `python-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/python//buffer-local-variables ()
  "Configure snippet engine for `python' mode."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika/python//buffer-local-mappings ()
  "Configure keymap for `python' mode."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika/python//minor-modes ()
  "Configure minor modes for `python' mode."
  (eldoc-mode          +1)

  (anaconda-mode       +1)
  (anaconda-eldoc-mode +1)
  (pyenv-mode          +1))

(defun serika/python//snippet-engine ()
  "Configure snippet engine for `python' mode."
  (serika/yasnippet/activate))

(defun serika/python//syntax-checking ()
  "Configure syntax checking for `python' mode."
  (flycheck-mode           +1)

  (flycheck-cask-setup))

(defun serika/python//auto-completion ()
  "Configure auto completion for `python' mode."
  (setq company-backends '(company-anaconda))

  (company-mode +1))

(defun serika/python//interface ()
  "Configure interface for `python' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun init ()
  "Configure `python-mode'."
  (serika/python//auto-mode-alist)

  (add-hook 'python-mode-hook #'serika/python//evil)
  (add-hook 'python-mode-hook #'serika/python//buffer-local-variables)
  (add-hook 'python-mode-hook #'serika/python//buffer-local-mappings)
  (add-hook 'python-mode-hook #'serika/python//minor-modes)

  (add-hook 'python-mode-hook #'serika/python//snippet-engine)
  (add-hook 'python-mode-hook #'serika/python//syntax-checking)
  (add-hook 'python-mode-hook #'serika/python//auto-completion)

  (add-hook 'python-mode-hook #'serika/python//interface))
