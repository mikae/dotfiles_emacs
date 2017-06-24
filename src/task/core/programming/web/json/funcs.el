;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'json-mode)
(require 'web-beautify)

(defun serika/json//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))
(defun serika/json//keymap ()
  "Configure `json-mode-map'."
  (setq json-mode-map (make-sparse-keymap)))

(defun serika/json//evil ()
  "Configure `evil' for `json-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-set-initial-state 'json-mode 'normal))

(defun serika/json//buffer-local-variables ()
  "Configure buffer-local variables for `json'."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika/json//buffer-local-mappings ()
  "Configure buffer-local mappings for `json'."
  (evil-local-set-key 'normal (kbd "=") 'web-beautify-js))

(defun serika/json//snippet-engine ()
  "Configure snippet engine for `json'."
  (yas-minor-mode +1)
  (yas-recompile-all)
  (yas-reload-all))

(defun serika/json//syntax-checking ()
  "Configure syntax checking engine for `json'."
  (flycheck-mode +1))

(defun serika/json//auto-completion ()
  "Configure auto completion engine for `json'."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika/json//interface ()
  "Configure interface for `json'."
  (rainbow-delimiters-mode +1)
  (rainbow-mode            +1)
  (linum-mode              +1)

  (setq show-trailing-whitespace 1))

(defun serika/json//prettify-symbols ()
  "Configure `prettify-symbols' for `json'."
  (prettify-symbols-mode +1)
  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `json-mode'."
  (serika/json//auto-mode-alist)
  (serika/json//keymap)

  (add-hook 'json-mode-hook 'serika/json//evil)
  (add-hook 'json-mode-hook 'serika/json//buffer-local-variables)
  (add-hook 'json-mode-hook 'serika/json//buffer-local-mappings)

  (add-hook 'json-mode-hook 'serika/json//syntax-checking)
  (add-hook 'json-mode-hook 'serika/json//snippet-engine)
  (add-hook 'json-mode-hook 'serika/json//auto-completion)

  (add-hook 'json-mode-hook 'serika/json//interface)
  (add-hook 'json-mode-hook 'serika/json//prettify-symbols))
