;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/emacs-lisp/p ()
  "Return t if major mode is `emacs-lisp-mode'."
  (eq 'emacs-lisp-mode
      major-mode))

;; Local
(defun serika-l/emacs-lisp//interface ()
  "Configure interface for `emacs-lisp' mode."
  )

;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :package-list '(flycheck-cask)
                           :name         'emacs-lisp)

  (serika-c/eg/add-many 'emacs-lisp
                        ("require")
                        (lambda ()
                          (require 'func-keymap)
                          (require 'func-buffer)
                          (require 'flycheck-cask))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)))

                        ("settings smartparens")
                        (lambda ()
                          (sp-local-pair 'emacs-lisp-mode "(" ")"))

                        ("keymap")
                        (lambda ()
                          (setq --serika-emacs-lisp-mode-map emacs-lisp-mode-map)
                          (serika-f/keymap/create emacs-lisp-mode-map
                                                  "C-t =" #'evil-indent
                                                  "C-t /" #'evilnc-comment-or-uncomment-lines
                                                  "C-t e" #'yas-expand))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list
                                             (serika-f/evil/create-activator
                                              (setq evil-shift-width 2))
                                             #'serika-f/smartparens/activate
                                             (serika-f/settings/create-configurator tab-width      2
                                                                                    truncate-lines t)

                                             #'serika-f/yasnippet/activate
                                             (serika-f/flycheck/create-activator
                                              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                                              (flycheck-cask-setup))

                                             (serika-f/company/create-activator
                                              (setq-local company-backends '(company-elisp)))

                                             #'serika-f/eldoc/activate
                                             #'serika-f/flycheck/activate

                                             #'serika-f/settings/show-trailing-whitespaces
                                             #'serika-f/linum-relative/activate
                                             #'serika-f/rainbow-delimiters/activate

                                             (serika-f/prettify-symbols/create-configurator "lambda" ?λ
                                                                                            ">="     ?≤
                                                                                            "<="     ?≥)
                                             (serika-f/purpose/use-layout "emacs-lisp.purpose-layout")

                                             #'serika-f/flycheck/create))
                            ;; `lisp-interaction-mode' is inherited from `emacs-lisp-mode',
                            ;; so, predicate was added
                            (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                          callback
                                                          #'serika-f/emacs-lisp/p))
                          (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                        #'serika-f/neotree/create
                                                        (serika-f/func/create-ander #'serika-f/neotree/not-exists-p
                                                                                    #'serika-f/emacs-lisp/p))
                          (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                        (serika-f/func/bind 'serika-f/buffer/focus-to
                                                                            'emacs-lisp-mode)
                                                        (lambda ()
                                                          (and (not (eq major-mode
                                                                        'emacs-lisp-mode))
                                                               (not (eq major-mode
                                                                        'lisp-interaction-mode))))))))
