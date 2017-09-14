;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `lisp' modes."
  (serika-c/eg/add-many-by-name 'lisp
                                ("keymap")
                                (lambda ()
                                  (func/keymap/save lisp-mode-map)
                                  (func/keymap/create lisp-mode-map))))
