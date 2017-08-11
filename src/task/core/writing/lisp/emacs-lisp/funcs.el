;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/emacs-lisp/p ()
  "Return t if major mode is `emacs-lisp-mode'."
  (eq 'emacs-lisp-mode
      major-mode))

;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(flycheck-cask)
                           :name         'emacs-lisp)

  (serika-c/eg/add-many
   'emacs-lisp
   ("require")
   (lambda ()
     (require 'flycheck-cask))

   ("settings")
   (lambda ()
     (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)))

   ("settings w-purpose")
   (lambda ()
     (add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . edit)))

   ("settings smartparens")
   (lambda ()
     (serika-f/smartparens/load 'emacs-lisp-mode)
     )

   ("keymap")
   (lambda ()
     (func/keymap/create emacs-lisp-mode-map
                         "C-t =" #'evil-indent
                         "C-t /" #'evilnc-comment-or-uncomment-lines
                         "C-t e" #'yas-expand))

   ("hook")
   (lambda ()
     (func/hook/add-oncely
      'emacs-lisp-mode-hook
      (func/func/construct
       ;; `lisp-interaction-mode' is inherited from `emacs-lisp-mode',
       ;; so, predicate was added
       (func/func/predicated
        (func/func/construct (serika-f/settings/create-configurator tab-width      2
                                                                    truncate-lines t)
                             (serika-f/evil/create-activator
                              (setq evil-shift-width 2))
                             #'serika-f/smartparens/activate
                             #'serika-f/aggressive-indent/activate

                             #'serika-f/yasnippet/activate
                             (serika-f/flycheck/create-activator
                              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                              (flycheck-cask-setup))
                             (serika-f/company/create-activator
                              (setq-local company-backends '(company-elisp)))
                             #'serika-f/eldoc/activate
                             #'serika-f/ggtags/activate
                             #'serika-f/flycheck/activate
                             #'serika-f/projectile/try-activate

                             #'serika-f/settings/show-trailing-whitespaces
                             #'serika-f/linum-relative/activate
                             #'serika-f/rainbow-delimiters/activate
                             #'serika-f/highlight-symbol/activate
                             (serika-f/prettify-symbols/create-loader "emacs-lisp")
                             (serika-f/purpose/use-layout "emacs-lisp.purpose-layout")

                             #'serika-f/flycheck/create
                             (func/func/predicated #'serika-f/treemacs/create
                                                   #'serika-f/treemacs/not-exists-p))
        #'serika-f/emacs-lisp/p)
       (func/func/predicated
        (func/func/bind 'func/buffer/focus-to
                        'emacs-lisp-mode)
        (lambda ()
          (not (func/buffer/check-modes 'emacs-lisp-mode
                                        'lisp-interaction-mode)))))))))
