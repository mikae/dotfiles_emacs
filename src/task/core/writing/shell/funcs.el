;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/sh//evil ()
  "Configure evil for `sh-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/sh//buffer-local-variables()
  "Configure buffer-local variables for `sh'."
  (setq tab-width 4)
  (setq truncate-lines t))

(defun serika-l/sh//snippet-engine ()
  "Configure snippet engine for `sh'."
  (serika-f/yasnippet/activate))

(defun serika-l/sh//interface ()
  "Configure interface for `sh'."
  (rainbow-delimiters-mode +1)
  (serika-f/linum-relative/activate)

  (setq show-trailing-whitespace 1))

(defun serika-l/sh//prettify-symbols ()
  "Configure `prettify-symbols' for `sh'."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add-many 'sh
                        ("require")
                        (lambda ()
                          (require 'func-hook))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.sh\\'"  . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshenv\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshprofile\\'" . sh-mode)))

                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(sh-mode . (("bash" . "bash  %path")
                                                                           ("zsh"  . "zsh   %path")))))

                        ("keymap")
                        (lambda ()
                          (serika-f/keymap/create sh-mode-map
                                                  "C-c c" #'multi-compile-run))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list
                                             #'serika-l/sh//evil
                                             #'serika-l/sh//buffer-local-variables

                                             #'serika-l/sh//snippet-engine
                                             #'serika-l/sh//interface
                                             #'serika-l/sh//prettify-symbols
                                             #'serika-f/eldoc/activate
                                             #'serika-f/flycheck/activate

                                             (serika-f/purpose/use-layout "sh.purpose-layout")

                                             #'serika-f/flycheck/create))
                            (serika-f/hook/add 'sh-mode-hook callback))

                          (serika-f/hook/add-predicated 'sh-mode-hook
                                                        #'serika-f/treemacs/create
                                                        #'serika-f/treemacs/not-exists-p))))
