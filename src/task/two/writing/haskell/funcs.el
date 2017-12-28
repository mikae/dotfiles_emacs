;;; package --- Summary
;;; Commentary:
;;; Code:

;; (defun serika-f/haskell/setup-buffer ()
;;   "Setup haskell buffers."
;;   (message "%s" major-mode)
;;   (when (func/buffer/check-modes 'haskell-mode)
;;     (func/var/ensure-local tab-width             2
;;                            haskell-indent-spaces 2
;;                            truncate-lines        t)

;;     (serika-f/evil/activate :evil-shift-width 2
;;                             :evil-state       'normal)
;;     (serika-f/smartparens/activate)
;;     ;; (serika-f/aggressive-indent/activate)

;;     (serika-f/flycheck/activate)
;;     (serika-f/yasnippet/activate)
;;     (serika-f/company/activate :backends-set '((company-ghc :with company-dabbrev-code)
;;                                                (company-files
;;                                                 company-yasnippet)))
;;     ;; (serika-f/eldoc/activate)

;;     (serika-f/settings/show-trailing-whitespaces)
;;     (serika-f/linum-relative/activate)
;;     (serika-f/rainbow-delimiters/activate)
;;     (serika-f/highlight-symbol/activate)
;;     ;; (serika-f/prettify-symbols/activate :name "haskell")
;;     ))

;; (defun init ()
;;   "Configure haskell."
;;   (serika-c/eg/add-install :type         'package
;;                            :name         'haskell-mode
;;                            :package-list '(haskell-mode)
;;                            :parents      '("install haskell"))

;;   (serika-c/eg/add-install :type       'git
;;                            :name       'hindent
;;                            :src        "https://github.com/mikae/hindent"
;;                            :extra-path '("elisp")
;;                            :parents    '("install haskell"))

;;   (add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-8.0.2/ghc-mod-5.8.0.0/elisp")

;;   (serika-c/eg/add-install :type       'git
;;                            :name       'company-ghc
;;                            :src        "https://github.com/mikae/company-ghc"
;;                            :parents    '("install haskell"))

;;   (serika-c/eg/add-many-by-name 'haskell
;;     ("require")
;;     (func/func/require 'haskell-mode
;;                        'hindent
;;                        'ghc
;;                        'company-ghc)

;;     ("settings")
;;     (serika-f/settings/register-ft 'haskell-mode
;;                                    "\\.hs\\'")

;;     ("settings smartparens")
;;     (progn
;;       (sp-local-pair 'haskell-mode "("    ")")
;;       (sp-local-pair 'haskell-mode "{"    "}")
;;       (sp-local-pair 'haskell-mode "["    "]")
;;       (sp-local-pair 'haskell-mode "\""   "\"")
;;       (sp-local-pair 'haskell-mode "\\\"" "\\\"")
;;       (sp-local-pair 'haskell-mode "\\'"  "\\'"))

;;     ("keymap")
;;     (progn
;;       (func/keymap/save haskell-mode-map)
;;       (func/keymap/create haskell-mode-map
;;                           "TAB" #'yas-expand

;;                           "C-t ="   #'hindent-reformat-decl
;;                           "C-t C-=" #'haskell-mode-stylish-buffer
;;                           "C-t /"   #'evilnc-comment-or-uncomment-lines))

;;     ("hook")
;;     (func/hook/add 'haskell-mode-hook
;;                    #'serika-f/haskell/setup-buffer)))
