;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `expand-region'."
  (serika-c/eg/add-install :type 'package
                           :name 'expand-region
                           :package-list '(expand-region)
                           :parents '("install evil"))
  (serika-c/eg/add-many-by-name 'expand-region
                                ("require")
                                (func/func/requirer 'expand-region)

                                ("keymap evil visual")
                                (lambda ()
                                  (func/keymap/define evil-visual-state-map
                                                      "TAB" #'er/expand-region)))
  )
