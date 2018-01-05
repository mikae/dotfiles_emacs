;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `iedit'."
  (serika-c/eg/add-install :type 'git
                           :name 'iedit
                           :src  "https://github.com/shinkiley/iedit")

  (serika-c/eg/add-many-by-name 'iedit
    ("require")
    (func/func/require 'iedit)

    ("keymap")
    (progn
      (func/keymap/save   iedit-mode-keymap)
      (func/keymap/create iedit-mode-keymap)

      (func/keymap/save   iedit-mode-occurrence-keymap)
      (func/keymap/create iedit-mode-occurrence-keymap))))
