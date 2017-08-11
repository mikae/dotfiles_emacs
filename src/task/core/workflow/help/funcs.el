;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `help-mode'."
  (serika-c/eg/add-many 'help
                        ("keymap")
                        (lambda ()
                          (func/keymap/save   help-mode-map)
                          (func/keymap/create help-mode-map
                                              "q" #'func/buffer/kill))

                        ("hook")
                        (lambda ()
                          (add-hook 'help-mode-hook (serika-f/evil/create-activator
                                                     (evil-motion-state))))))
