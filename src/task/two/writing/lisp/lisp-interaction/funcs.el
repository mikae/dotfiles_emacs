;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/lisp-interaction/setup-buffer ()
  "Configure `lisp-interaction' buffers."
  (when (eq major-mode
            'lisp-interaction-mode)
    (setq tab-width      2
          truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)

    (serika-f/company/activate)

    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    (serika-f/prettify-symbols/activate :name "lisp-interaction")))

;; Init
(defun init ()
  "Configure `lisp-interaction-mode'."
  (serika-c/eg/add-many-by-name 'lisp-interaction
                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'lisp-interaction-mode "("    ")")
                                  (sp-local-pair 'lisp-interaction-mode "{"    "}")
                                  (sp-local-pair 'lisp-interaction-mode "["    "]")
                                  (sp-local-pair 'lisp-interaction-mode "\""   "\"")
                                  (sp-local-pair 'lisp-interaction-mode "`"    "'")
                                  (sp-local-pair 'lisp-interaction-mode "\\\"" "\\\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create lisp-interaction-mode-map
                                                      "TAB"      #'yas-expand

                                                      "C-t ="    #'evil-indent
                                                      "C-t /"    #'evilnc-comment-or-uncomment-lines
                                                      "C-c e"    #'eval-last-sexp
                                                      "C-x C-s"  #'ignore))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'lisp-interaction-mode-hook
                                                 #'serika-f/lisp-interaction/setup-buffer))))
