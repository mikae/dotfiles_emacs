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
    (func/var/ensure-local tab-width      2
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate :disabled-checkers '(emacs-lisp-checkdoc))
    (serika-f/yasnippet/activate)
    (flycheck-cask-setup)
    (serika-f/company/activate)

    (serika-f/eldoc/activate)
    ;; (serika-f/ggtags/activate)
    ;; (serika-f/projectile/try-activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    (serika-f/prettify-symbols/activate :name "emacs-lisp")))

;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(flycheck-cask)
                           :name         'emacs-lisp)

  (serika-c/eg/add-many-by-name 'emacs-lisp
    ("require")
    (progn
      (require 'flycheck-cask))

    ("settings")
    (progn
      (serika-f/settings/register-ft 'emacs-lisp-mode "\\.el\\'"))


    ("settings smartparens")
    (progn
      (sp-local-pair 'emacs-lisp-mode "("    ")")
      (sp-local-pair 'emacs-lisp-mode "{"    "}")
      (sp-local-pair 'emacs-lisp-mode "["    "]")
      (sp-local-pair 'emacs-lisp-mode "\""   "\"")
      (sp-local-pair 'emacs-lisp-mode "`"    "'")
      (sp-local-pair 'emacs-lisp-mode "\\\"" "\\\""))

    ("keymap")
    (progn
      (func/keymap/create emacs-lisp-mode-map
                          "TAB" #'yas-expand

                          "C-t =" #'evil-indent
                          "C-t /" #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (progn
      (func/hook/add-oncely 'emacs-lisp-mode-hook
                            #'serika-f/emacs-lisp/setup-buffer))))
