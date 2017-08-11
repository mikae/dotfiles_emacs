;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add-many 'sh
                        ("require")
                        (lambda ()
                          ())

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.sh\\'"  . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshenv\\'" . sh-mode))
                          (add-to-list 'auto-mode-alist '("\\.zshprofile\\'" . sh-mode)))

                        ("settings w-purpose")
                        (lambda ()
                          (add-to-list 'purpose-user-mode-purposes '(sh-mode . edit)))

                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(sh-mode . (("bash" . "bash  %path")
                                                                         ("zsh"  . "zsh   %path")))))

                        ("keymap")
                        (lambda ()
                          (func/keymap/create sh-mode-map
                                              "C-c c" #'multi-compile-run))

                        ("hook")
                        (lambda ()
                          ;; todo: fix this
                          )))
