;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'lua-mode)
(require 'company-lua)

(defun serika/lua//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(defun serika/lua//evil ()
  "Configure `evil' for `lua-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/lua//keymap ()
  "Configure `lua-mode-map'."
  (setq lua-mode-map (make-sparse-keymap)))

(defun serika/lua//buffer-local-variables()
  "Configure buffer-local variables for `lua'."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika/lua//buffer-local-mappings()
  "Configure buffer-local mappings for `lua'."
  (evil-local-set-key 'normal (kbd "=")   'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

;; IDE-like
(defun serika/lua//snippet-engine ()
  "Configure snippet engine for `lua'."
  (serika/yasnippet/activate))

(defun serika/lua//syntax-checking ()
  "Configure syntax checking for `lua'."
  (flycheck-mode +1))

(defun serika/lua//auto-completion ()
  "Configure auto completion for `lua'."
  (setq-local company-backends '(company-lua))

  (company-mode +1))

;; Interface
(defun serika/lua//interface ()
  "Configure interface for `lua'."
  (rainbow-delimiters-mode +1)
  (linum-mode              +1)

  (setq show-trailing-whitespace 1))

(defun serika/lua//prettify-symbols ()
  "Configure `prettify-symbols' for `lua'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `lua-mode'."
  (serika/lua//auto-mode-alist)
  (serika/lua//keymap)

  (add-hook 'lua-mode-hook 'serika/lua//evil)
  (add-hook 'lua-mode-hook 'serika/lua//buffer-local-variables)
  (add-hook 'lua-mode-hook 'serika/lua//buffer-local-mappings)

  (add-hook 'lua-mode-hook 'serika/lua//syntax-checking)
  (add-hook 'lua-mode-hook 'serika/lua//snippet-engine)
  (add-hook 'lua-mode-hook 'serika/lua//auto-completion)

  (add-hook 'lua-mode-hook 'serika/lua//interface)
  (add-hook 'lua-mode-hook 'serika/lua//prettify-symbols))
