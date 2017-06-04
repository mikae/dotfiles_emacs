;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'lua-mode)
(require 'company-lua)

;; Functions
(defun serika/lua/evil ()
  "Configure `evil' for `lua-mode'."
  (evil-set-initial-state 'lua-mode 'normal))

(defun serika/lua/keymap ()
  "Configure `lua-mode-map'."
  (setq lua-mode-map (make-sparse-keymap)))

(defun serika/lua/buffer-local-variables()
  "Configure buffer-local variables for `lua'."
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (setq truncate-lines t))

(defun serika/lua/buffer-local-mappings()
  "Configure buffer-local mappings for `lua'."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

;; IDE-like
(defun serika/lua/snippet-engine ()
  "Configure snippet engine for `lua'."
  (yas-minor-mode +1)
  (yas-recompile-all)
  (yas-reload-all))

(defun serika/lua/syntax-checking ()
  "Configure syntax checking for `lua'."
  (flycheck-mode +1))

(defun serika/lua/auto-completion ()
  "Configure auto completion for `lua'."
  (setq-local company-backends '(company-lua))

  (company-mode +1))

;; Interface
(defun serika/lua/interface ()
  "Configure interface for `lua'."
  (rainbow-delimiters-mode +1)
  (linum-mode              +1)

  (setq show-trailing-whitespace 1))

(defun serika/lua/prettify-symbols()
  "Configure `prettify-symbols' for `lua'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))
