;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/python/setup-buffer ()
  "Setup python buffers"
  (when (eq major-mode
            'python-mode)
    (setq tab-width      4
          truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 4
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)

    (serika-f/ycmd/activate)
    (serika-f/flycheck/activate)
    (serika-f/company/activate :backends-set '(company-ycmd))

    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

;; Init
(defun init ()
  "Configure `python-mode'."
  (serika-c/eg/add-install :type 'git
                           :name 'virtualenvwrapper
                           :src  "https://github.com/shinkiley/virtualenvwrapper.el")

  (serika-c/eg/add-many-by-name 'python
    ("require")
    (func/func/require 'python
                       'virtualenvwrapper)

    ("settings")
    (serika-f/settings/register-ft 'python-mode
                                   "\\.py\\'")

    ("settings multi-compile")
    (serika-f/multi-compile/configure 'python-mode
                                      "python" "python %path"
                                      "python2" "python2 %path"
                                      "python3" "python3 %path")

    ("settings smartparens")
    (progn
      (sp-local-pair 'python-mode "("    ")")
      (sp-local-pair 'python-mode "{"    "}")
      (sp-local-pair 'python-mode "["    "]")
      (sp-local-pair 'python-mode "\""   "\"")
      (sp-local-pair 'python-mode "`"    "'")
      (sp-local-pair 'python-mode "\\\"" "\\\""))

    ("settings spaceline")
    (progn
      (spaceline-define-segment python-virtualenvwrapper
        "The current virtual environment."
        (or (and venv-current-name (format "(%s)" venv-current-name))
            nil))

      (serika-f/spaceline/compile-default-with-extras python
        ()
        (python-virtualenvwrapper)))

    ("keymap")
    (progn
      (func/keymap/save python-mode-map)

      (func/keymap/create python-mode-map
        "TAB" #'yas-expand

        ;; arst
        "C-c a" #'multi-compile-run

        "C-c r a" #'venv-workon
        "C-c r A" #'venv-deactivate
        "C-c r r" #'venv-mkvirtualenv
        "C-c r R" #'venv-rmvirtualenv
        "C-c r d" #'venv-lsvirtualenv

        "C-t =" #'evil-indent
        "C-t /" #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (progn
      (func/hook/add 'python-mode-hook
                     #'serika-f/python/setup-buffer)
      (func/hook/add 'python-mode-hook
                     (lambda ()
                       (setq mode-line-format
                             '("%e" (:eval (spaceline-ml-python))))))))

  (serika-c/eg/add-many-by-name 'virtualenvwrapper
    ("settings python")
    (progn
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)
      (setq venv-location (func/system/getenv "$WORKON_HOME"
                                              "~/.local-data/.virtualenvs")))

    ("settings spaceline")
    (progn
      )))
