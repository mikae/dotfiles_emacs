;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/lua//require ()
  (require 'lua-mode)
  (require 'company-lua))

(defun serika-g/lua//settings ()
  "Configure `lua'."
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(defun serika-g/lua//keymap ()
  "Configure `lua-mode-map'."
  (setq lua-mode-map (make-sparse-keymap)))

;; Local
(defun serika-l/lua//evil ()
  "Configure `evil' for `lua-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/lua//buffer-local-variables()
  "Configure buffer-local variables for `lua'."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika-l/lua//buffer-local-mappings()
  "Configure buffer-local mappings for `lua'."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines)
  (evil-local-set-key 'normal (kbd "C-t e") 'yas-expand))

(defun serika-l/lua//snippet-engine ()
  "Configure snippet engine for `lua'."
  (serika-f/yasnippet/activate))

(defun serika-l/lua//syntax-checking ()
  "Configure syntax checking for `lua'."
  (flycheck-mode +1))

(defun serika-l/lua//auto-completion ()
  "Configure auto completion for `lua'."
  (setq-local company-backends '(company-lua))

  (company-mode +1))

;; Interface
(defun serika-l/lua//interface ()
  "Configure interface for `lua'."
  (rainbow-delimiters-mode +1)
  (linum-mode              +1)

  (setq show-trailing-whitespace 1))

(defun serika-l/lua//prettify-symbols ()
  "Configure `prettify-symbols' for `lua'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `lua-mode'."

  (serika-c/eg/add-install :package-list '(f s)
                           :name         'lua)

  (serika-c/eg/add :parents '("require")
                   :name    'lua
                   :func    #'serika-g/lua//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'lua
                   :func    #'serika-g/lua//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'lua
                   :func    #'serika-g/lua//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'lua
		   :func    (lambda ()
			      (add-hook 'lua-mode-hook 'serika-l/lua//evil)
			      (add-hook 'lua-mode-hook 'serika-l/lua//buffer-local-variables)
			      (add-hook 'lua-mode-hook 'serika-l/lua//buffer-local-mappings)

			      (add-hook 'lua-mode-hook 'serika-l/lua//syntax-checking)
			      (add-hook 'lua-mode-hook 'serika-l/lua//snippet-engine)
			      (add-hook 'lua-mode-hook 'serika-l/lua//auto-completion)

			      (add-hook 'lua-mode-hook 'serika-l/lua//interface)
			      (add-hook 'lua-mode-hook 'serika-l/lua//prettify-symbols))))
