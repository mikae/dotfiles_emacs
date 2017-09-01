;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/json//evil ()
  "Configure `evil' for `json-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-set-initial-state 'json-mode 'normal))

(defun serika-l/json//buffer-local-variables ()
  "Configure buffer-local variables for `json'."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/json//buffer-local-mappings ()
  "Configure buffer-local mappings for `json'."
  (evil-local-set-key 'normal (kbd "=") 'web-beautify-js))

(defun serika-l/json//snippet-engine ()
  "Configure snippet engine for `json'."
  (serika-f/yasnippet/activate))

(defun serika-l/json//syntax-checking ()
  "Configure syntax checking engine for `json'."
  (flycheck-mode +1))

(defun serika-l/json//auto-completion ()
  "Configure auto completion engine for `json'."
  (auto-complete-mode +1)

  (setq ac-sources '(
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika-l/json//interface ()
  "Configure interface for `json'."
  (rainbow-delimiters-mode +1)
  (rainbow-mode            +1)
  (serika-f/linum-relative/activate)

  (setq show-trailing-whitespace 1))

(defun serika-l/json//prettify-symbols ()
  "Configure `prettify-symbols' for `json'."
  (prettify-symbols-mode +1)
  (setq prettify-symbols-alist ()))

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
                                  (serika-f/settings/register-ft 'json-mode "\\.json\\'"))

                                ("keymap")
                                (lambda ()
                                  (setq --json-mode-map json-mode-map)
                                  (func/keymap/create json-mode-map))

                                ("hook")
                                (lambda ()
                                  (add-hook 'json-mode-hook 'serika-l/json//evil)
                                  (add-hook 'json-mode-hook 'serika-l/json//buffer-local-variables)
                                  (add-hook 'json-mode-hook 'serika-l/json//buffer-local-mappings)

                                  (add-hook 'json-mode-hook 'serika-l/json//syntax-checking)
                                  (add-hook 'json-mode-hook 'serika-l/json//snippet-engine)
                                  (add-hook 'json-mode-hook 'serika-l/json//auto-completion)

                                  (add-hook 'json-mode-hook 'serika-l/json//interface)
                                  (add-hook 'json-mode-hook 'serika-l/json//prettify-symbols))))
