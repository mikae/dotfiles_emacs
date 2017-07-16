;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/scheme//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode)))

;; Local
(defun serika-l/scheme//buffer-local-variables ()
  "Configure local variables for `scheme' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/scheme//buffer-local-mappings ()
  "Configure local mappings for `scheme' mode."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines)
  (evil-local-set-key 'normal (kbd "C-t e") 'yas-expand))

(defun serika-l/scheme//evil ()
  "Configure `evil' for `scheme-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-set-initial-state 'scheme-mode 'normal))

(defun serika-l/scheme//snippet-engine ()
  "Configure snippet engine for `scheme' mode."
  (serika-f/yasnippet/activate))

(defun serika-l/scheme//syntax-checking ()
  "Configure syntax checking for `scheme' mode."
  (flycheck-mode           +1))

(defun serika-l/scheme//auto-completion ()
  "Configure auto completion for `scheme' mode."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika-l/scheme//interface ()
  "Configure interface for `scheme' mode."
  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1)

  (setq show-trailing-whitespace +1))

(defun serika-l/scheme//prettify-symbols ()
  "Configure prettify symbols for `scheme' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

;; Init
(defun init ()
  "Configure `scheme-mode'."
  (serika-g/scheme//auto-mode-alist)

  (add-hook 'scheme-mode-hook 'serika-l/scheme//evil)
  (add-hook 'scheme-mode-hook 'serika-l/scheme//buffer-local-variables)
  (add-hook 'scheme-mode-hook 'serika-l/scheme//buffer-local-mappings)

  (add-hook 'scheme-mode-hook 'serika-l/scheme//snippet-engine)
  (add-hook 'scheme-mode-hook 'serika-l/scheme//syntax-checking)
  (add-hook 'scheme-mode-hook 'serika-l/scheme//auto-completion)

  (add-hook 'scheme-mode-hook 'serika-l/scheme//interface)
  (add-hook 'scheme-mode-hook 'serika-l/scheme//prettify-symbols))
