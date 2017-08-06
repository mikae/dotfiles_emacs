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
     (serika-f/keymap/create emacs-lisp-mode-map
                             "C-t =" #'evil-indent
                             "C-t /" #'evilnc-comment-or-uncomment-lines
                             "C-t e" #'yas-expand))

   ("hook")
   (lambda ()
     (serika-f/hook/add-oncely
      'emacs-lisp-mode-hook
      (serika-f/func/construct
       ;; `lisp-interaction-mode' is inherited from `emacs-lisp-mode',
       ;; so, predicate was added
       (serika-f/func/predicated
        (serika-f/func/construct (serika-f/settings/create-configurator tab-width      2
                                                                        truncate-lines t)
                                 (serika-f/evil/create-activator
                                  (setq evil-shift-width 2))
                                 #'serika-f/smartparens/activate

                                 #'serika-f/yasnippet/activate
                                 (serika-f/flycheck/create-activator
                                  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
                                  (flycheck-cask-setup))
                                 (serika-f/company/create-activator
                                  (setq-local company-backends '(company-elisp)))
                                 #'serika-f/eldoc/activate
                                 #'serika-f/ggtags/activate
                                 #'serika-f/flycheck/activate

                                 #'serika-f/settings/show-trailing-whitespaces
                                 #'serika-f/linum-relative/activate
                                 #'serika-f/rainbow-delimiters/activate
                                 (serika-f/prettify-symbols/create-loader "emacs-lisp")
                                 (serika-f/purpose/use-layout "emacs-lisp.purpose-layout")

                                 #'serika-f/flycheck/create
                                 (serika-f/func/predicated #'serika-f/neotree/create
                                                           #'serika-f/neotree/not-exists-p))
        #'serika-f/emacs-lisp/p)
       (serika-f/func/predicated
        (serika-f/func/bind 'serika-f/buffer/focus-to
                            'emacs-lisp-mode)
        (lambda ()
          (not (serika-f/buffer/check-modes 'emacs-lisp-mode
                                            'lisp-interaction-mode)))))))))
