;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/emacs-lisp/p ()
  "Return t if major mode is `emacs-lisp-mode'."
  (eq 'emacs-lisp-mode
      major-mode))

(defun serika-f/emacs-lisp/setup-buffer ()
  "Setup `emacs-lisp' buffer."
  (when (eq major-mode
            'emacs-lisp-mode)
    (setq tab-width      2
          truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)

    (serika-f/flycheck/activate :disabled-checkers '(emacs-lisp-checkdoc))
    (flycheck-cask-setup)

    (serika-f/company/activate :backends '(company-elisp))

    (serika-f/eldoc/activate)
    (serika-f/ggtags/activate)
    ;; (serika-f/projectile/try-activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    (serika-f/prettify-symbols/activate :name "emacs-lisp")

    ;; ;; create flycheck window
    ;; (when (and (serika-f/flycheck/activated-p)
    ;;            (serika-f/flycheck/not-exists-p))
    ;;   (serika-f/flycheck/create))

    ;; autofocus to `emacs-lisp' buffer
    (when (not (func/buffer/check-modes 'emacs-lisp-mode
                                        'lisp-interaction-mode))
      (func/buffer/focus-to 'emacs-lisp-mode))
    )
  )

;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(flycheck-cask)
                           :name         'emacs-lisp)

  (serika-c/eg/add-many-by-name 'emacs-lisp
                                ("require")
                                (lambda ()
                                  (require 'flycheck-cask))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'emacs-lisp-mode "\\.el\\'"))


                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'emacs-lisp-mode "("    ")")
                                  (sp-local-pair 'emacs-lisp-mode "{"    "}")
                                  (sp-local-pair 'emacs-lisp-mode "["    "]")
                                  (sp-local-pair 'emacs-lisp-mode "\""   "\"")
                                  (sp-local-pair 'emacs-lisp-mode "`"    "'")
                                  (sp-local-pair 'emacs-lisp-mode "\\\"" "\\\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create emacs-lisp-mode-map
                                                      "C-t =" #'evil-indent
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines
                                                      "C-t e" #'yas-expand))

                                ("hook")
                                (lambda ()
                                  (func/hook/add-oncely 'emacs-lisp-mode-hook
                                                        #'serika-f/emacs-lisp/setup-buffer))))
