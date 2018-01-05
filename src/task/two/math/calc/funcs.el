;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `calc-mode'."
  (serika-c/eg/add-many-by-name 'calc
    ("require")
    (func/func/require 'calc)

    ("keymap")
    (progn
      (func/keymap/save   calc-mode-map)
      ;; (func/keymap/create calc-mode-map)
      )

    ("global-keymap")
    (func/keymap/define-global "<C-m> c a l c" #'calc)))
