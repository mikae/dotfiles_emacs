;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/js2//keymap ()
  "Configure `js2-mode-map'."
  )

;; Local
(defun serika-l/js2//evil ()
  "Configure `evil' for `js'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/js2//buffer-local-variables ()
  "Configure snippet engine for `js' mode."
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq truncate-lines t))

(defun serika-l/js2//syntax-checking ()
  "Configure syntax checking for `js' mode."
  (flycheck-mode           +1))

(defun serika-l/js2//snippet-engine ()
  "Configure snippet engine for `js' mode."
  (serika-f/yasnippet/activate)
  (serika-f/emmet/activate))

(defun serika-l/js2//auto-completion ()
  "Configure auto completion for `js' mode."
  (setq ac-sources '(
                      ac-source-filename
                      ac-source-dictionary
                      ac-source-files-in-current-dir))
  (ac-js2-mode))

;; Interface
(defun serika-l/js2//interface ()
  "Configure interface for `js' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

(defun serika-l/js2//prettify-symbols ()
  "Configure prettify symbols for `js' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure Emacs for `js'-programming."
  (serika-c/eg/add-install :package-list '(js2-mode
                                           ac-js2)
                           :name         'js2)
  (serika-c/eg/add-many-by-name 'js2
                        ("require")
                        (lambda ()
                          (require 'js2-mode)
                          (require 'skewer-mode)
                          (require 'ac-js2))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(js2-mode . (("Execute" . "node %path")))))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save js2-mode-map)
                          (func/keymap/create js2-mode-map
                                              "C-c e e" 'skewer-eval-last-expression
                                              "C-c e d" 'skewer-eval-defun
                                              "C-c e a" 'skewer-load-buffer
                                              "C-c e r" 'run-skewer

                                              "C-c c c" 'multi-compile-run
                                              "C-c c d" (lambda ()
                                                          (interactive)
                                                          (func/buffer/kill-by-major-mode 'compilation-mode))

                                              "C-t ="   'evil-indent
                                              "C-t +"   'web-beautify-js
                                              "C-t /"   'evilnc-comment-or-uncomment-lines
                                              "C-t e"   'yas-expand
                                              "C-t E"   'serika-f/emmet/expand))
                        ("hook")
                        (lambda ()
                          (dolist (callback (list
                                             #'serika-l/js2//evil
                                             #'serika-l/js2//buffer-local-variables

                                             #'serika-l/js2//syntax-checking
                                             #'serika-l/js2//snippet-engine
                                             #'serika-f/skewer/activate
                                             #'serika-f/eldoc/activate
                                             #'serika-f/flycheck/create

                                             ;; bug:
                                             ;; `https://github.com/ScottyB/ac-js2/issues/18'
                                             ;; (add-hook 'js2-mode-hook #'serika-l/js2//auto-completion)

                                             #'serika-l/js2//interface
                                             #'serika-l/js2//prettify-symbols

                                             #'serika-f/flycheck/create))
                            (func/hook/add 'js2-mode-hook callback))

                          )))
