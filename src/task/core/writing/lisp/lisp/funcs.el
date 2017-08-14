;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/lisp/setup-buffer ()
  "Configure `lisp' buffers."
  (when (eq major-mode
            'lisp-mode)
    (setq tab-width      2
          truncate-lines t)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)

    (serika-f/flycheck/activate :disabled-checkers '(emacs-lisp-checkdoc))

    (serika-f/eldoc/activate)
    (serika-f/ggtags/activate)
    (serika-f/projectile/try-activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    (serika-f/prettify-symbols/activate :name "lisp")))

;; Init
(defun init ()
  "Configure `lisp-mode'."
  (serika-c/eg/add-many 'lisp-mode
                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.lsp\\'" . lisp-mode)))

                        ("hook")
                        (lambda ()
                          (func/hook/add 'lisp-mode-hook
                                         #'serika-f/lisp/setup-buffer))))
