;;; package --- Summary
;;; Commentary:
;;; Code:

;; todo:
;; add `https://github.com/foretagsplatsen/emacs-js/blob/master/emacs-js.el'

;; Funcs
(defun serika-f/js2/activate-minor ()
  "Activate minor mode `js2'."
  (js2-minor-mode +1))

(defun serika-f/js2/setup-buffer ()
  "Configure `js2-mode' buffer."
  (when (eq major-mode 'js2-mode)
    (setq tab-width        2
          truncate-lines   t)

    (setq js2-global-externs '("define" "require" "app")
          js2-pretty-multiline-declarations nil
          js2-include-node-externs t
          js-indent-level 2)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/js2/activate-minor)
    (serika-f/js2-refactor/activate)
    (serika-f/yasnippet/activate)
    (when buffer-file-name
      (serika-f/flycheck/activate))
    (serika-f/company/activate :backends '(company-tern
                                           company-gtags
                                           company-yasnippet))
    (serika-f/tern/activate)
    (serika-f/eldoc/activate)
    (serika-f/ggtags/activate)
    (serika-f/projectile/try-activate)

    (serika-f/purpose/load-layout "js2.purpose-layout")

    (when yas-minor-mode
      (serika-f/flycheck/create))

    (serika-f/treemacs/create)

    (unless (func/buffer/check-modes 'js2-mode)
      (func/buffer/focus-to 'js2-mode))

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)

    (serika-f/prettify-symbols/activate :name "js2")))


;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(js2-mode)
                           :name         'js2)

  (serika-c/eg/add-many 'js2
                        ("require")
                        (lambda ()
                          (require 'js2-mode)
                          (require 'js2-refactor)
                          (require 'xref-js2)
                          (require 'tern)
                          (require 'company-tern)
                          )

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

                        ("settings w-purpose")
                        (lambda ()
                          (add-to-list 'purpose-user-mode-purposes '(js2-mode . edit)))

                        ("settings smartparens")
                        (lambda ()
                          (sp-local-pair 'js2-mode "("    ")")
                          (sp-local-pair 'js2-mode "{"    "}")
                          (sp-local-pair 'js2-mode "["    "]")
                          (sp-local-pair 'js2-mode "\""   "\"")
                          (sp-local-pair 'js2-mode "'"    "'")
                          (sp-local-pair 'js2-mode "\\\"" "\\\"")
                          (sp-local-pair 'js2-mode "\\'" "\\'"))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   js2-mode-map)
                          (func/keymap/create js2-mode-map
                                              "C-t =" #'evil-indent
                                              "C-t /" #'evilnc-comment-or-uncomment-lines
                                              "C-t e" #'yas-expand
                                              "C-t E" #'serika-f/emmet/expand))

                        ("hook")
                        (lambda ()
                          (func/hook/add-oncely 'js2-mode-hook
                                                #'serika-f/js2/setup-buffer))))
