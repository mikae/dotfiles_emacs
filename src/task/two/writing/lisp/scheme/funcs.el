;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/scheme//buffer-local-variables ()
  "Configure local variables for `scheme' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/scheme//buffer-local-mappings ()
  "Configure local mappings for `scheme' mode."
  )

(defun serika-l/scheme//evil ()
  "Configure `evil' for `scheme-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/scheme//auto-completion ()
  "Configure auto completion for `scheme' mode."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika-l/scheme//interface ()
  "Configure interface for `scheme' mode."
  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate)

  (setq show-trailing-whitespace +1))

(defun serika-l/scheme//prettify-symbols ()
  "Configure prettify symbols for `scheme' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

;; Init
(defun init ()
  "Configure `scheme-mode'."
  (serika-c/eg/add-many-by-name 'scheme
                                ("require")
                                (lambda ()
                                  (require 'scheme))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'scheme-mode "\\.scm\\'"))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create scheme-mode-map
                                                      "C-t =" 'evil-indent
                                                      "C-t /" 'evilnc-comment-or-uncomment-lines
                                                      "C-t e" 'yas-expand))

                                ("hook")
                                (lambda ()
                                  (dolist (callback (list
                                                     'serika-l/scheme//evil
                                                     'serika-l/scheme//buffer-local-variables
                                                     'serika-l/scheme//buffer-local-mappings

                                                     'serika-f/yasnippet/activate
                                                     'serika-f/flycheck/activate
                                                     'serika-l/scheme//auto-completion

                                                     'serika-l/scheme//interface
                                                     'serika-l/scheme//prettify-symbols))
                                    (func/hook/add 'scheme-mode-hook callback)))))
