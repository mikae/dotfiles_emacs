;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/python//require ()
  "Require modules for `python'."
  )

(defun serika-g/python//settings ()
  "Configure `python'."
  )

(defun serika-g/python|virtualenvwrapper//settings ()
  "Configure `virtualenwrapper'."
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location (serika-f/system/getenv "PATH_VIRTUALENVS")))

(defun serika-g/python//keymap ()
  "Configure `python-mode-map'."
  )

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
  (serika-f/linum-relative/activate))

;; Init
(defun init ()
  "Configure `python-mode'."
  (serika-c/eg/add-install :package-list '(anaconda-mode
                                           company-anaconda
                                           pyenv-mode
                                           virtualenvwrapper)
                           :name         'python)
  (serika-c/eg/add-many 'python
                        ("require")
                        (lambda ()
                          (require 'anaconda-mode)
                          (require 'virtualenvwrapper)
                          (require 'company-anaconda)
                          (require 'pyenv-mode))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

                        ("keymap")
                        (lambda ()
                          (serika-f/keymap/save python-mode-map
                                                anaconda-mode-map)

                          (serika-f/keymap/create python-mode-map
                                                  "C-c v w" #'venv-workon
                                                  "C-c v d" #'venv-deactivate
                                                  "C-c v m" #'venv-mkvirtualenv
                                                  "C-c v r" #'venv-rmvirtualenv
                                                  "C-c v l" #'venv-lsvirtualenv
                                                  "C-c v c" #'venv-cdvirtualenv
                                                  "C-c v y" #'venv-cpvirtualenv)
                          (serika-f/keymap/create anaconda-mode-map))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/python//evil
                                                  #'serika-l/python//buffer-local-variables
                                                  #'serika-l/python//buffer-local-mappings
                                                  #'serika-l/python//minor-modes

                                                  #'serika-l/python//snippet-engine
                                                  #'serika-l/python//syntax-checking
                                                  #'serika-l/python//auto-completion
                                                  #'serika-f/eldoc/activate

                                                  #'serika-l/python//interface))
                            (serika-f/hook/add 'python-mode-hook callback))))

  (serika-c/eg/add :parents '("settings python")
                   :name    'virtualenvwrapper
                   :func    #'serika-g/python|virtualenvwrapper//settings))
