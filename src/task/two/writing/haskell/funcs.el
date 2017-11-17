;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/haskell/setup-buffer ()
  "Setup haskell buffers."
  (message "%s" major-mode)
  (when (func/buffer/check-modes 'haskell-mode)
    (func/var/ensure-local tab-width             2
                           haskell-indent-spaces 2
                           truncate-lines        t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    ;; (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate)
    ;; (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    ;; (serika-f/prettify-symbols/activate :name "haskell")
    ))

(defun init ()
  "Configure haskell."
  (serika-c/eg/add-install :type         'package
                           :name         'haskell-mode
                           :package-list '(haskell-mode)
                           :parents      '("install haskell"))

  (serika-c/eg/add-many-by-name 'haskell
    ("require")
    (func/func/require 'haskell-mode)

    ("settings")
    (serika-f/settings/register-ft 'haskell-mode
                                   "\\.hs\\'")

    ("keymap")
    (progn
      (func/keymap/save haskell-mode-map)
      (func/keymap/create haskell-mode-map
                          "TAB" #'yas-expand

                          "C-t =" #'evil-indent
                          "C-t /" #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (func/hook/add 'haskell-mode-hook
                   #'serika-f/haskell/setup-buffer)))
