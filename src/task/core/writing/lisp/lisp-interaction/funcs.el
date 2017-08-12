;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `lisp-interaction-mode'."
  (serika-c/eg/add-many 'lisp-interaction
                        ("keymap")
                        (lambda ()
                          (func/keymap/create lisp-interaction-mode-map
                                              "C-t ="    #'evil-indent
                                              "C-t /"    #'evilnc-comment-or-uncomment-lines
                                              "C-c e"    #'eval-last-sexp
                                              "C-x C-s"  #'ignore))
                        ("hook")
                        (lambda ()
                          (func/hook/add 'lisp-interaction-mode-hook
                                         (func/func/construct (serika-f/settings/create-configurator tab-width       2
                                                                                                     truncate-lines  t)
                                                              (serika-f/evil/create-activator
                                                               (setq evil-shift-width 2))

                                                              #'serika-f/yasnippet/activate
                                                              #'serika-f/eldoc/activate
                                                              (serika-f/company/create-activator
                                                               (setq-local company-backends '(company-elisp)))


                                                              #'serika-f/highlight-symbol/activate
                                                              #'serika-f/settings/show-trailing-whitespaces
                                                              #'serika-f/linum-relative/activate
                                                              #'serika-f/rainbow-delimiters/activate
                                                              (serika-f/prettify-symbols/create-loader "lisp-interaction"))))))
