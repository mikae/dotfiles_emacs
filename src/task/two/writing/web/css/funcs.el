;;; package --- Summary
;;; Commentary:
;;; Code:

;; Hook
(defun serika-f/css/setup-buffer ()
  "Setup `css' buffers."
  (func/var/ensure-local shift-width 2
                         truncate-lines t)
  (serika-f/evil/activate :evil-state       'normal
                          :evil-shift-width 2)
  (serika-f/emmet/activate)
  (serika-f/yasnippet/activate)
  (serika-f/flycheck/activate)
  (serika-f/company/activate :backends-set '(company-css))

  (serika-f/rainbow-delimiters/activate)
  (serika-f/rainbow-mode/activate)
  (serika-f/linum-relative/activate)
  (serika-f/settings/show-trailing-whitespaces)
  ;; (serika-f/prettify-symbols/activate)
  )

;; Init
(defun init ()
  "Configure `css'."
  (serika-c/eg/add-many-by-name 'css
                                ("require")
                                (lambda ()
                                  (require 'css-mode))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'css-mode "\\.css\\'"))

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'js2-mode "{"    "}")
                                  (sp-local-pair 'js2-mode "\""   "\"")
                                  (sp-local-pair 'js2-mode "'"    "'"))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save css-mode-map)
                                  (func/keymap/create css-mode-map
                                                      "C-t e" #'yas-expand
                                                      "C-t E" #'serika-f/emmet/expand
                                                      "C-t =" #'evil-indent
                                                      "C-t +" #'web-beautify-css
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'css-mode-hook
                                                 #'serika-f/css/setup-buffer))))
