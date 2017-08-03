;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/emacs-lisp/p ()
  "Return t if major mode is `emacs-lisp-mode'."
  (eq 'emacs-lisp-mode
      major-mode))

;; Local
(defun serika-l/emacs-lisp//buffer-local-variables ()
  "Configure snippet engine for `emacs-lisp' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/emacs-lisp//interface ()
  "Configure interface for `emacs-lisp' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

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

                                             #'serika-l/emacs-lisp//buffer-local-variables

                                             #'serika-f/yasnippet/activate
                                             (serika-f/flycheck/create-activator
                                              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                                              (flycheck-cask-setup))

                                             (serika-f/company/create-activator
                                              (setq-local company-backends '(company-elisp)))

                                             #'serika-f/eldoc/activate
                                             #'serika-f/flycheck/activate

                                             #'serika-l/emacs-lisp//interface
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
