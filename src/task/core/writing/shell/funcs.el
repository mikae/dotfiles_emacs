;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/sh//require ()
  "Require modules for `sh'."
  (require 'func-hook))

(defun serika-g/sh//keymap ()
  "Configure `sh-mode-map'."
  (setq sh-mode-map (let ((map (make-sparse-keymap)))
                      map)))

(defun serika-g/sh//settings ()
  "Configure `sh-mode'."
  ;; `auto-mode-alist'
  (add-to-list 'auto-mode-alist '("\\.sh\\'"  . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.zshenv\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.zshprofile\\'" . sh-mode)))

;; Local
(defun serika-l/sh//evil ()
  "Configure evil for `sh-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/sh//buffer-local-variables()
  "Configure buffer-local variables for `sh'."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika-l/sh//snippet-engine ()
  "Configure snippet engine for `sh'."
  (serika-f/yasnippet/activate))

(defun serika-l/sh//interface ()
  "Configure interface for `sh'."
  (rainbow-delimiters-mode +1)
  (serika-f/linum-relative/activate)

  (setq show-trailing-whitespace 1))

(defun serika-l/sh//prettify-symbols ()
  "Configure `prettify-symbols' for `sh'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add :parents '("require")
                   :name    'sh
                   :func    #'serika-g/sh//require)

  (serika-c/eg/add :parents '("keymap")
                   :name    'sh
                   :func    #'serika-g/sh//keymap)

  (serika-c/eg/add :parents '("settings"
                              "settings w-purpose")
                   :name    'sh
                   :func    #'serika-g/sh//settings)

  (serika-c/eg/add :parents '("hook")
                   :name    'sh
                   :func    (lambda ()
                              (let ((hook 'sh-mode-hook))
                                (serika-f/add-hook hook 'serika-l/sh//evil)
                                (serika-f/add-hook hook 'serika-l/sh//buffer-local-variables)

                                (serika-f/add-hook hook 'serika-l/sh//snippet-engine)
                                (serika-f/add-hook hook 'serika-l/sh//interface)
                                (serika-f/add-hook hook 'serika-l/sh//prettify-symbols)
                                (serika-f/add-hook hook 'serika-f/eldoc/activate)

                                (serika-f/add-hook hook (serika-f/purpose/use-layout "sh.purpose-layout"))

                                (serika-f/add-hook-predicated hook
                                                              #'serika-f/flycheck/create
                                                              #'serika-f/flycheck/not-exists-p)
                                (serika-f/add-hook-predicated hook
                                                              #'serika-f/neotree/create
                                                              #'serika-f/neotree/not-exists-p)))))
