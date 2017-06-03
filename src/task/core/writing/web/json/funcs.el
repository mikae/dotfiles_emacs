;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'json-mode)
(require 'web-beautify)

;; Functions
(defun serika/json/evil ()
  "Configure `evil' for `json-mode'."
  (evil-set-initial-state 'json-mode 'normal))

(defun serika/json/keymap ()
  "Configure `json-mode-map'."
  (setq json-mode-map (make-sparse-keymap)))

(defun serika/json/buffer-local-variables ()
  "Configure buffer-local variables for `json'."
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika/json/buffer-local-mappings ()
  "Configure buffer-local mappings for `json'."
  (evil-local-set-key 'normal (kbd "=") 'web-beautify-js))

(defun serika/json/snippet-engine ()
  "Configure snippet engine for `json'."
  (yas-minor-mode +1)
  (yas-recompile-all)
  (yas-reload-all))

(defun serika/json/syntax-checking ()
  "Configure syntax checking engine for `json'."
  (flycheck-mode +1))

(defun serika/json/auto-completion ()
  "Configure auto completion engine for `json'."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika/json/interface ()
  "Configure interface for `json'."
  (rainbow-delimiters-mode +1)
  (rainbow-mode            +1)
  (linum-mode              +1)

  (setq show-trailing-whitespace 1))

(defun serika/json/prettify-symbols ()
  "Configure `prettify-symbols' for `json'."
  (prettify-symbols-mode +1)
  (setq prettify-symbols-alist ()))
