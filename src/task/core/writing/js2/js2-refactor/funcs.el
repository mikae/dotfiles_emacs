;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/js2-refactor/activate ()
  "Activate `js2-refactor-mode'."
  (js2-refactor-mode +1))

;; Init
(defun init ()
  "Configure `js2-refactor'."
  (serika-c/eg/add-install :type 'package
                           :name 'js2-refactor
                           :package-list '(js2-refactor))

  (serika-c/eg/add-many-by-name 'js2-refactor
                                ("require js2")
                                (func/func/requirer 'js2-refactor)

                                ("keymap js2")
                                (lambda ()
                                  (func/keymap/save   js2-refactor-mode-map)
                                  (func/keymap/create js2-refactor-mode-map)))
  )
