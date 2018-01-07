;;; package --- Summary
;;; Commentary:
;;; Code:

;; todo:
;; add `https://github.com/foretagsplatsen/emacs-js/blob/master/emacs-js.el'
;; Funcs
(defun serika-f/js2-tern/activate ()
  "Activate `tern-mode'."
  (tern-mode +1))

;; Hook
(defun serika-f/js2/setup-buffer ()
  "Configure `js2-mode' buffer."
  (when (eq major-mode 'js2-mode)
    (func/var/ensure-local tab-width        2
                           truncate-lines   t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/js2-tern/activate)
    (serika-f/yasnippet/activate)
    (serika-f/emmet/activate)
    (serika-f/ycmd/activate)
    (serika-f/company/activate :backends-set '((company-ycmd)
                                               (company-files company-yasnippet)))
    (serika-f/eldoc/activate)

    (when buffer-file-name
      (serika-f/flycheck/activate))

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)

    (serika-f/prettify-symbols/activate :name "js2")))

;; Init
(defun init ()
  "Configure `emacs-lisp-mode'."
  (dolist (--package '((js2-mode     . "https://github.com/shinkiley/js2-mode")
                       (company-tern . "https://github.com/shinkiley/company-tern")))
    (serika-c/eg/add-install :type    'git
                             :name    (car --package)
                             :src     (cdr --package)
                             :parents '("install js2")))

  (serika-c/eg/add-install :type       'git
                           :name       'tern
                           :src        "https://github.com/shinkiley/tern"
                           :extra-path '("emacs")
                           :post-hook  "npm install"
                           :parents    '("install js2"))

  (serika-c/eg/add-many-by-name 'js2
    ("require")
    (func/func/require 'js2-mode
                       'company-tern)

    ("settings")
    (progn
      ;; filetype
      (serika-f/settings/register-ft 'js2-mode
                                     "\\.js\\'")

      ;; js
      (setq js-indent-level                   2)

      ;; js2-mode
      (setq js2-global-externs                '("define" "require" "app")
            js2-pretty-multiline-declarations nil
            js2-include-node-externs          t))

    ("settings multi-compile")
    (serika-f/multi-compile/configure 'js2-mode
                                      "node" "node %path")

    ("settings smartparens")
    (progn
      (sp-local-pair 'js2-mode "("    ")")
      (sp-local-pair 'js2-mode "{"    "}")
      (sp-local-pair 'js2-mode "["    "]")
      (sp-local-pair 'js2-mode "\""   "\"")
      (sp-local-pair 'js2-mode "'"    "'")
      (sp-local-pair 'js2-mode "\\\"" "\\\"")
      (sp-local-pair 'js2-mode "\\'" "\\'"))

    ("keymap")
    (progn
      ;; `js2-mode'
      (func/keymap/save   js2-mode-map)
      (func/keymap/create js2-mode-map
        "TAB"     #'yas-expand

        "C-t ="   #'evil-indent
        "C-t +"   #'web-beautify-js
        "C-t /"   #'evilnc-comment-or-uncomment-lines
        "C-t E"   #'serika-f/emmet/expand

        ;; "C-c a a"   #'find-other-file
        "C-c a r" #'dumb-jump-go
        "C-c a R" #'dumb-jump-back

        "C-c r a" #'multi-compile-run
        "C-c r A" (lambda ()
                    (interactive)
                    (func/buffer/kill-by-major-mode 'compilation-mode)))

      ;; `tern'
      (func/keymap/save   tern-mode-keymap)
      (func/keymap/create tern-mode-keymap))

    ("hook")
    (func/hook/add-oncely 'js2-mode-hook
                          #'serika-f/js2/setup-buffer)))
