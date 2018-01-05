;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/ggtags/activate ()
  "Activate `ggtags' in current buffer."
  (ggtags-mode))

;; Init
(defun init ()
  (serika-c/eg/add-install :type    'git
                           :name    'ggtags
                           :src     "https://github.com/shinkiley/ggtags")

  (serika-c/eg/add-many-by-name 'ggtags
    ("require")
    (progn
      (func/keymap/create ggtags-mode-map)
      (func/keymap/create ggtags-global-mode-map)
      (func/keymap/create ggtags-mode-prefix-map)
      (func/keymap/create ggtags-navigation-map)
      (func/keymap/create ggtags-navigation-mode-map)
      (func/keymap/create ggtags-mode-line-project-keymap)
      (func/keymap/create ggtags-highlight-tag-map)
      (func/keymap/create ggtags-view-search-history-mode-map)
      (func/keymap/create ggtags-view-tag-history-mode-map)

      (require 'ggtags))))
