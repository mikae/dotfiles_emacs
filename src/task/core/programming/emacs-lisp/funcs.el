;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'flycheck-cask)

(defun serika/emacs-lisp//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)))

(defun serika/emacs-lisp//buffer-local-variables ()
  "Configure snippet engine for `emacs-lisp' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika/emacs-lisp//buffer-local-mappings ()
  "Configure keymap for `emacs-lisp' mode."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika/emacs-lisp//evil ()
  "Configure `evil' for `emacs-lisp-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/emacs-lisp//snippet-engine ()
  "Configure snippet engine for `emacs-lisp' mode."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

(defun serika/emacs-lisp//syntax-checking ()
  "Configure syntax checking for `emacs-lisp' mode."
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-mode           +1)

  (flycheck-cask-setup))

(defun serika/emacs-lisp//auto-completion ()
  "Configure auto completion for `emacs-lisp' mode."
  (setq-local company-backends '(company-elisp))

  (company-mode +1))

(defun serika/emacs-lisp//auto-pairing ()
  "Configure auto completion for `emacs-lisp' mode."
  (electric-pair-mode +1))

(defun serika/emacs-lisp//interface ()
  "Configure interface for `emacs-lisp' mode."

  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun serika/emacs-lisp//prettify-symbols ()
  "Configure prettify symbols for `emacs-lisp' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '("lambda" . ?λ) prettify-symbols-alist)
  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika/emacs-lisp//auto-mode-alist)

  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//evil)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//buffer-local-variables)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//buffer-local-mappings)

  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//snippet-engine)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//syntax-checking)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//auto-completion)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//auto-pairing)

  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//interface)
  (add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp//prettify-symbols))
