;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure haskell."
  (serika-c/eg/add-install :type         'package
                           :name         'haskell-mode
                           :package-list '(haskell-mode)
                           :parents      '("install haskell"))

  (serika-c/eg/add-many-by-name 'haskell
                                ("require")
                                (func/func/requirer 'haskell-mode)

                                ("settings")
                                (lambda ()
                                  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

                                ("keymap")
                                (lambda ()
                                  )
                                ))

