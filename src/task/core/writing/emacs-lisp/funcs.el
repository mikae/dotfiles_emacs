;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika/emacs-lisp/buffer-local-variables ()
  "Configure snippet engine for `emacs-lisp' mode."
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika/emacs-lisp/buffer-local-mappings ()
  "Configure keymap for `emacs-lisp' mode."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika/emacs-lisp/evil ()
  "Configure `evil' for `emacs-lisp-mode'."
  (evil-set-initial-state 'emacs-lisp-mode 'normal))

(defun serika/emacs-lisp/snippet-engine ()
  "Configure snippet engine for `emacs-lisp' mode."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

(defun serika/emacs-lisp/syntax-checking ()
  "Configure syntax checking for `emacs-lisp' mode."
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-mode           +1)

  (flycheck-cask-setup))

(defun serika/emacs-lisp/auto-completion ()
  "Configure auto completion for `emacs-lisp' mode."
  (setq-local company-backends '(company-elisp))

  (company-mode +1))

(defun serika/emacs-lisp/auto-pairing ()
  "Configure auto completion for `emacs-lisp' mode."
  (electric-pair-mode +1))

(defun serika/emacs-lisp/interface ()
  "Configure interface for `emacs-lisp' mode."

  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun serika/emacs-lisp/prettify-symbols ()
  "Configure prettify symbols for `emacs-lisp' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '("lambda" . ?λ) prettify-symbols-alist)
  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))
