;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/js/setup-buffer ()
  "Configure `js-mode' buffer."
  (when (eq major-mode 'js-mode)
    (setq tab-width        2
          truncate-lines   t)

    (setq js-indent-level 2)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/yasnippet/activate)

    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)

    (serika-f/prettify-symbols/activate :name "js")))


;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-many-by-name 'js
    ("settings smartparens")
    (progn
      (sp-local-pair 'js-mode "("    ")")
      (sp-local-pair 'js-mode "{"    "}")
      (sp-local-pair 'js-mode "["    "]")
      (sp-local-pair 'js-mode "\""   "\"")
      (sp-local-pair 'js-mode "'"    "'")
      (sp-local-pair 'js-mode "\\\"" "\\\"")
      (sp-local-pair 'js-mode "\\'"  "\\'"))

    ("keymap")
    (progn
      (func/keymap/save   js-mode-map)
      (func/keymap/create js-mode-map
        "TAB"       #'yas-expand
        "C-t ="     #'evil-indent
        "C-t /"     #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (progn
      (func/hook/add 'js-mode-hook
                     #'serika-f/js/setup-buffer))))
