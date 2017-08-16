;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs

(defun serika-f/sh/setup-buffer ()
  "Configure sh buffers."
  (when (eq major-mode
            'sh-mode)
    (setq tab-width        2
          truncate-lines   t
          evil-shift-width 2)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)

    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/yasnippet/activate)
    (when buffer-file-name
      (serika-f/flycheck/activate))

    (serika-f/eldoc/activate)
    (serika-f/ggtags/activate)
    (serika-f/projectile/try-activate)

    (serika-f/company/activate :backends '(company-shell
                                           company-shell-env))

    (when yas-minor-mode
      (serika-f/flycheck/create))

    (unless (func/buffer/check-modes 'sh-mode)
      (func/buffer/focus-to 'sh-mode))

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)

    (serika-f/prettify-symbols/activate :name "sh")))

;; Init
(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add-many 'sh
                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.sh\\'"  . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshenv\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshprofile\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.xinit\\'" . sh-mode)))

                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(sh-mode . (("bash" . "bash  %path")
                                                                         ("zsh"  . "zsh   %path")))))

                        ("settings smartparens")
                        (lambda ()
                          (sp-local-pair 'sh-mode "("    ")")
                          (sp-local-pair 'sh-mode "{"    "}")
                          (sp-local-pair 'sh-mode "["    "]")
                          (sp-local-pair 'sh-mode "\""   "\"")
                          (sp-local-pair 'sh-mode "'"    "'")
                          (sp-local-pair 'sh-mode "\\\"" "\\\""))

                        ("keymap")
                        (lambda ()
                          (func/keymap/create sh-mode-map
                                              "C-c c" #'multi-compile-run

                                              "C-t =" #'evil-indent
                                              "C-t /" #'evilnc-comment-or-uncomment-lines
                                              "C-t e" #'yas-expand))

                        ("hook")
                        (lambda ()
                          (func/hook/add 'sh-mode-hook
                                         #'serika-f/sh/setup-buffer)
                          )))
