;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/python//require ()
  "Require modules for `python'."
  (require 'company-anaconda)
  (require 'pyenv-mode))

(defun serika-g/python//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(defun serika-g/python//keymap ()
  "Configure `python-mode-map'."
  (setq python-mode-map (make-sparse-keymap))

  ;; virtualenvwrapper
  (define-key python-mode-map (kbd "C-c v w") #'venv-workon)
  (define-key python-mode-map (kbd "C-c v d") #'venv-deactivate)
  (define-key python-mode-map (kbd "C-c v m") #'venv-mkvirtualenv)
  (define-key python-mode-map (kbd "C-c v r") #'venv-rmvirtualenv)
  (define-key python-mode-map (kbd "C-c v l") #'venv-lsvirtualenv)
  (define-key python-mode-map (kbd "C-c v c") #'venv-cdvirtualenv)
  (define-key python-mode-map (kbd "C-c v y") #'venv-cpvirtualenv))

;; Local
(defun serika-l/python//evil ()
  "Configure `evil' for `python-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/python//buffer-local-variables ()
  "Configure snippet engine for `python' mode."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika-l/python//buffer-local-mappings ()
  "Configure keymap for `python' mode."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines)
  (evil-local-set-key 'normal (kbd "C-t e") 'yas-expand))

(defun serika-l/python//minor-modes ()
  "Configure minor modes for `python' mode."
  (eldoc-mode          +1)

  (anaconda-mode       +1)
  (anaconda-eldoc-mode +1))

(defun serika-l/python//snippet-engine ()
  "Configure snippet engine for `python' mode."
  (serika-f/yasnippet/activate))

(defun serika-l/python//syntax-checking ()
  "Configure syntax checking for `python' mode."
  (flycheck-mode           +1)

  (flycheck-cask-setup))

(defun serika-l/python//auto-completion ()
  "Configure auto completion for `python' mode."
  (setq company-backends '(company-anaconda))

  (company-mode +1))

(defun serika-l/python//interface ()
  "Configure interface for `python' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

;; Init
(defun init ()
  "Configure `python-mode'."
  (serika-g/python//require)
  (serika-g/python//auto-mode-alist)
  (serika-g/python//keymap)

  (add-hook 'python-mode-hook #'serika-l/python//evil)
  (add-hook 'python-mode-hook #'serika-l/python//buffer-local-variables)
  (add-hook 'python-mode-hook #'serika-l/python//buffer-local-mappings)
  (add-hook 'python-mode-hook #'serika-l/python//minor-modes)

  (add-hook 'python-mode-hook #'serika-l/python//snippet-engine)
  (add-hook 'python-mode-hook #'serika-l/python//syntax-checking)
  (add-hook 'python-mode-hook #'serika-l/python//auto-completion)

  (add-hook 'python-mode-hook #'serika-l/python//interface))
