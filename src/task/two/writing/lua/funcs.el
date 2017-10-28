;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-f/lua//setup-buffer ()
  "Setup `lua' buffer."
  (when (eq major-mode
            'lua-mode)
    (func/var/ensure-local tab-width      4
                           truncate-lines t)

    (serika-f/evil/activate :evil-state       'normal
                            :evil-shift-width 4)

    (serika-f/yasnippet/activate)
    (serika-f/flycheck/activate)
    (serika-f/company/activate :backends-set '(company-lua))
    (serika-f/eldoc/activate)
    (serika-f/flycheck/activate)

    (serika-f/rainbow-delimiters/activate)
    (serika-f/linum-relative/activate)
    (serika-f/settings/show-trailing-whitespaces)
    ;; (serika-f/prettify-symbols/activate)
    ))

(defun init ()
  "Configure `lua-mode'."
  (serika-c/eg/add-install :type   'git
                           :name   'lua-mode
                           :src    "https://github.com/mikae/lua-mode"
                           :parents '("install lua"))

  (serika-c/eg/add-install :type   'git
                           :name   'company-lua
                           :src    "https://github.com/ptrv/company-lua"
                           :parents '("install lua"))

  (serika-c/eg/add-many-by-name 'lua
                                ("require")
                                (lambda ()
                                  (require 'lua-mode)
                                  (require 'company-lua))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'lua-mode "\\.lua\\'"))

                                ("settings multi-compile")
                                (lambda ()
                                  (add-to-list 'multi-compile-alist '(lua-mode . (("Execute" . "lua %path")))))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create lua-mode-map
                                                      "TAB" #'yas-expand

                                                      "C-c c" #'multi-compile-run
                                                      "C-t =" #'evil-indent
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines))
                                ("hook")
                                (lambda ()
                                  (func/hook/add 'lua-mode-hook #'serika-f/lua//setup-buffer))))
