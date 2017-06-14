;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/scheme//buffer-local-variables ()
  "Configure local variables for `scheme' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika/scheme//buffer-local-mappings ()
  "Configure local mappings for `scheme' mode."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika/scheme//evil ()
  "Configure `evil' for `scheme-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-set-initial-state 'scheme-mode 'normal))

(defun serika/scheme//snippet-engine ()
  "Configure snippet engine for `scheme' mode."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

(defun serika/scheme//syntax-checking ()
  "Configure syntax checking for `scheme' mode."
  (flycheck-mode           +1))

(defun serika/scheme//auto-completion ()
  "Configure auto completion for `scheme' mode."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika/scheme//interface ()
  "Configure interface for `scheme' mode."
  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1)

  (setq show-trailing-whitespace +1))

(defun serika/scheme//prettify-symbols ()
  "Configure prettify symbols for `scheme' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

(defun init ()
  "Configure `scheme-mode'."
  (add-hook 'scheme-mode-hook 'serika/scheme//evil)
  (add-hook 'scheme-mode-hook 'serika/scheme//buffer-local-variables)
  (add-hook 'scheme-mode-hook 'serika/scheme//buffer-local-mappings)

  (add-hook 'scheme-mode-hook 'serika/scheme//snippet-engine)
  (add-hook 'scheme-mode-hook 'serika/scheme//syntax-checking)
  (add-hook 'scheme-mode-hook 'serika/scheme//auto-completion)

  (add-hook 'scheme-mode-hook 'serika/scheme//interface)
  (add-hook 'scheme-mode-hook 'serika/scheme//prettify-symbols))
