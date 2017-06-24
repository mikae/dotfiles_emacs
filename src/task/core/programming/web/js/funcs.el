;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'js2-mode)
(require 'ac-js2)

;; Global
(defun serika/js//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; Local
(defun serika/js//evil ()
  "Configure `evil' for `js'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/js//buffer-local-variables ()
  "Configure snippet engine for `js' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika/js//buffer-local-mappings ()
  "Configure keymap for `js' mode."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika/js//syntax-checking ()
  "Configure syntax checking for `js' mode."
  (flycheck-mode           +1))

(defun serika/js//snippet-engine ()
  "Configure snippet engine for `js' mode."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

(defun serika/js//auto-completion ()
  "Configure auto completion for `js' mode."
  (setq ac-sources '(
                      ac-source-filename
                      ac-source-dictionary
                      ac-source-files-in-current-dir))
  (ac-js2-mode))

(defun serika/js//auto-pairing ()
  "Configure auto completion for `js' mode."
  (electric-pair-mode +1))

(defun serika/js//interface ()
  "Configure interface for `js' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun serika/js//prettify-symbols ()
  "Configure prettify symbols for `js' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure Emacs for `js'-programming."
  (serika/js//auto-mode-alist)

  (add-hook 'js2-mode-hook 'serika/js//evil)
  (add-hook 'js2-mode-hook 'serika/js//buffer-local-variables)
  (add-hook 'js2-mode-hook 'serika/js//buffer-local-mappings)

  (add-hook 'js2-mode-hook 'serika/js//syntax-checking)
  (add-hook 'js2-mode-hook 'serika/js//snippet-engine)
  (add-hook 'js2-mode-hook 'serika/js//auto-completion)
  (add-hook 'js2-mode-hook 'serika/js//auto-pairing)

  (add-hook 'js2-mode-hook 'serika/js//interface)
  (add-hook 'js2-mode-hook 'serika/js//prettify-symbols))
