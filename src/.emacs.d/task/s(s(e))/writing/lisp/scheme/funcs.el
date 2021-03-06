;;; package --- Summary
;;; Commentary:
;;; Code:

;; Hook
(defun serika-f/scheme//setup-buffer ()
  "Setup scheme buffers"
  (when (eq major-mode
            'scheme-mode)
    (func/var/ensure-local tab-width 2
                           truncate-lines t)
    (serika-f/evil/activate :evil-state       'normal
                            :evil-shift-width 2)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate)

    (serika-f/prettify-symbols/activate :name "scheme")
    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

;; Init
(defun init ()
  "Configure `scheme-mode'."
  (serika-c/eg/add-many-by-name 'scheme
    ("require")
    (func/func/require 'scheme)

    ("settings")
    (serika-f/settings/register-ft 'scheme-mode "\\.scm\\'")

    ("settings multi-compile")
    (add-to-list 'multi-compile-alist '(scheme-mode . (("mit-scheme" . "scheme"))))

    ("settings smartparens")
    (progn
      (sp-local-pair 'scheme-mode "("    ")")
      (sp-local-pair 'scheme-mode "{"    "}")
      (sp-local-pair 'scheme-mode "["    "]")
      (sp-local-pair 'scheme-mode "\""   "\"")
      (sp-local-pair 'scheme-mode "`"    "'")
      (sp-local-pair 'scheme-mode "\\\"" "\\\""))

    ("keymap")
    (func/keymap/create scheme-mode-map
      "TAB" 'yas-expand

      "C-c A" #'multi-compile-run

      "C-t =" 'evil-indent
      "C-t /" 'evilnc-comment-or-uncomment-lines)

    ("hook")
    (func/hook/add 'scheme-mode-hook
                   #'serika-f/scheme//setup-buffer)))
