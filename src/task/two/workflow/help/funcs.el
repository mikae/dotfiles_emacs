;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/help/setup-buffer ()
  "Setup `help' buffer."
  (serika-f/evil/activate :evil-state 'motion))

;; Init
(defun init ()
  "Configure `help-mode'."
  (serika-c/eg/add-many-by-name 'help
    ("keymap")
    (progn
      (func/keymap/save   help-mode-map)
      (func/keymap/create help-mode-map
        "q" #'func/buffer/kill))

    ("hook")
    (func/hook/add 'help-mode-hook
                   #'serika-f/help/setup-buffer)))
