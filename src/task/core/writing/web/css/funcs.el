;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/css//keymap ()
  "Configure `css-mode-map'"
  )

;; Local
(defun serika-l/css//evil ()
  "Configure `evil' for `css'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/css//buffer-local-variables ()
  "Configure buffer-local variables for `css' files."
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika-l/css//snippet-engine ()
  "Snippet-engine for `css'."
  (serika-f/emmet/activate)
  (serika-f/yasnippet/activate))

(defun serika-l/css//syntax-checking ()
  "Configure syntax checking for `web-mode' buffers with `css' engine."
  (flycheck-mode +1))

(defun serika-l/css//auto-completion ()
  "Configure syntax checking for `web-mode' buffers with `css' engine."
  (setq-local company-backends '(company-css))
  (company-mode +1))

(defun serika-l/css//interface ()
  "Configure interface for `web-mode' buffers with `css' engine."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (rainbow-mode                  +1)
  (serika-f/linum-relative/activate)

  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `css'."
  (serika-c/eg/add-many 'css
                        ("require")
                        (lambda ()
                          (require 'css-mode))

                        ("keymap")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode)))

                        ("settings")
                        (lambda ()
                          (setq --serika-css-mode-map css-mode-map)
                          (serika-f/keymap/create css-mode-map
                                                  "C-t e" #'yas-expand
                                                  "C-t E" #'serika-f/emmet/expand
                                                  "C-t =" #'evil-indent
                                                  "C-t +" #'web-beautify-css
                                                  "C-t /" #'evilnc-comment-or-uncomment-lines))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/css//evil
                                                  #'serika-l/css//buffer-local-variables
                                                  #'serika-l/css//snippet-engine
                                                  #'serika-l/css//syntax-checking
                                                  #'serika-l/css//auto-completion
                                                  #'serika-f/eldoc/activate
                                                  #'serika-l/css//interface))
                            (serika-f/hook/add 'css-mode-hook
                                               callback)))))
