;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/ggtags/activate ()
  "Activate `ggtags' in current buffer."
  (ggtags-mode))

;; Init
(defun init ()
  (serika-c/eg/add-install :type         'package
                           :name         'ggtags
                           :package-list '(ggtags))

  (serika-c/eg/add-many 'ggtags
                        ("require")
                        (lambda ()
                          (require 'ggtags))

                        ("keymap")
                        (lambda ()
                          ;; (func/keymap/save   ggtags-mode-map)
                          ;; (func/keymap/create ggtags-mode-map
                          ;;                     "C-x t")
                          )))
