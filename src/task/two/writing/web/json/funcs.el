;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-f/json//setup-buffer ()
  "Setup `json' buffer"
  (when (eq major-mode
            'json-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)
    (serika-f/company/activate :backends-set '((company-keywords company-files)))

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

;; Init
(defun init ()
  "Configure `json-mode'."
  (serika-c/eg/add-install :package-list '(json-mode)
                           :name         'json)

  (serika-c/eg/add-many-by-name 'json
                                ("require")
                                (lambda ()
                                  (require 'json-mode)
                                  (require 'web-beautify))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'json-mode
                                                                 "\\.json\\'"
                                                                 "\\.babelrc\\'"))

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
                                  (func/keymap/save   json-mode-map)
                                  (func/keymap/create json-mode-map
                                                      "C-t ="   #'evil-indent
                                                      "C-t C-=" #'web-beautify-js))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'json-mode-hook #'serika-f/json//setup-buffer))))
