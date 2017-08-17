;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/help/setup-buffer ()
  "Setup `help' buffer."
  )

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
                          (add-hook 'help-mode-hook #'serika-f/help/setup-buffer))))
