;;; package --- Summary
;;; Commentary:
;;; Code:

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
  (serika-f/linum-relative/activate)

  (setq show-trailing-whitespace 1))

(defun serika-l/lua//prettify-symbols ()
  "Configure `prettify-symbols' for `lua'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `lua-mode'."
  (serika-c/eg/add-install :package-list '(f s)
                           :name         'lua)
  (serika-c/eg/add-many 'lua
                        ("require")
                        (lambda ()
                          (require 'lua-mode)
                          (require 'company-lua))
                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))
                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(lua-mode . (("Execute" . "lua %path")))))
                        ("keymap")
                        (lambda ()
                          (serika-f/keymap/create lua-mode-map
                                                  "C-c c" #'multi-compile-run
                                                  "C-t =" #'evil-indent
                                                  "C-t /" #'evilnc-comment-or-uncomment-lines
                                                  "C-t e" #'yas-expand))
                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/lua//evil
                                                  #'serika-l/lua//buffer-local-variables

                                                  #'serika-l/lua//syntax-checking
                                                  #'serika-l/lua//snippet-engine
                                                  #'serika-l/lua//auto-completion
                                                  #'serika-f/eldoc/activate
                                                  #'serika-f/flycheck/activate

                                                  #'serika-l/lua//interface
                                                  #'serika-l/lua//prettify-symbols

                                                  (serika-f/purpose/use-layout "lua.purpose-layout")

                                                  #'serika-f/flycheck/create))
                            (serika-f/hook/add 'lua-mode-hook callback))

                          (serika-f/hook/add-predicated 'lua-mode-hook
                                                        #'serika-f/neotree/create
                                                        #'serika-f/neotree/not-exists-p))))
