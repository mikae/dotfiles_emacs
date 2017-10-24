;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/anaconda/activate ()
  "Activate anaconda mode"
  (interactive)
  (anaconda-mode +1))

(defun serika-f/anaconda/eldoc-activate ()
  "Activate `anaconda-eldoc-mode'."
  (interactive)
  (anaconda-eldoc-mode))

(defun serika-f/python/setup-buffer ()
  "Setup python buffers"
  (when (eq major-mode
            'python-mode)
    (let ((--current-buffer (current-buffer)))
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
      ;; (serika-f/anaconda/activate)

      (serika-f/eldoc/activate)
      ;; (serika-f/anaconda/eldoc-activate)
      (serika-f/ggtags/activate)
      ;; (serika-f/projectile/try-activate)

      (serika-f/settings/show-trailing-whitespaces)
      (serika-f/linum-relative/activate)
      (serika-f/rainbow-delimiters/activate)
      (serika-f/highlight-symbol/activate)
      ;; (serika-f/prettify-symbols/activate :name "python")

      ;; autofocus to previous buffers
      (when (not (func/buffer/check-modes 'python-mode))
        (switch-to-buffer --current-buffer)
        (func/buffer/focus-to 'python-mode)))))

;; Init
(defun init ()
  "Configure `python-mode'."
  (serika-c/eg/add-install :package-list '(anaconda-mode
                                           company-anaconda
                                           pyenv-mode
                                           virtualenvwrapper)
                           :name         'python)

  (serika-c/eg/add-many-by-name 'python
                                ("require")
                                (func/func/require 'virtualenvwrapper
                                                   'pyenv-mode
                                                   ;; 'anaconda-mode
                                                   ;; 'company-anaconda
                                                   )

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'python-mode
                                                                 "\\.py\\'")

                                  ;; `virtualenvwrapper'
                                  (venv-initialize-interactive-shells)
                                  (venv-initialize-eshell)
                                  (setq venv-location
                                        (func/system/getenv "PATH_VIRTUALENVS")))

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'python-mode "("    ")")
                                  (sp-local-pair 'python-mode "{"    "}")
                                  (sp-local-pair 'python-mode "["    "]")
                                  (sp-local-pair 'python-mode "\""   "\"")
                                  (sp-local-pair 'python-mode "`"    "'")
                                  (sp-local-pair 'python-mode "\\\"" "\\\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save python-mode-map
                                                    ;; anaconda-mode-map
                                                    )

                                  (func/keymap/create python-mode-map
                                                      "C-t =" #'evil-indent
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines
                                                      "C-t e" #'yas-expand
                                                      ;; "C-c v w" #'venv-workon
                                                      ;; "C-c v d" #'venv-deactivate
                                                      ;; "C-c v m" #'venv-mkvirtualenv
                                                      ;; "C-c v r" #'venv-rmvirtualenv
                                                      ;; "C-c v l" #'venv-lsvirtualenv
                                                      ;; "C-c v c" #'venv-cdvirtualenv
                                                      ;; "C-c v y" #'venv-cpvirtualenv
                                                      )
                                  ;; (func/keymap/create anaconda-mode-map)
                                  )

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'python-mode-hook
                                                 #'serika-f/python/setup-buffer))))
