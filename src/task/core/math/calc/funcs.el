;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `calc-mode'."
  (serika-c/eg/add-many 'calc
                        ("keymap")
                        (lambda ()
                          ;; (func/keymap/save   calc-mode-map)
                          ;; (func/keymap/create calc-mode-map)
                          )

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "<C-m> c a l c" #'calc))))
