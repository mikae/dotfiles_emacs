;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `iedit'."
  (serika-c/eg/add-install :type 'git
                           :name 'iedit
                           :src  "https://github.com/mikae/iedit")

  (serika-c/eg/add-many-by-name 'iedit
    ("require")
    (func/func/require 'iedit)

    ("keymap")
    (progn
      (func/keymap/save   iedit-mode-keymap)
      (func/keymap/create iedit-mode-keymap)

      (func/keymap/save   iedit-mode-occurrence-keymap)
      (func/keymap/create iedit-mode-occurrence-keymap))

    ("settings evil")
    (evil-define-state iedit
      "Iedit state"
      :tag "<IE>"
      :suppress-keymap t)

    ("keymap evil")
    (func/keymap/define evil-iedit-state-map
                        ;; qwfwp
                        "q" #'iedit-mode
                        "Q" #'evil-iedit-input-state
                        "w" #'iedit-prev-occurrence
                        "W" #'iedit-next-occurrence
                        "f" #'iedit-toggle-selection

                        "C-SPC" #'evil-normal-state)

    ("keymap evil normal")
    (func/keymap/define evil-normal-state-map
                        "l" #'evil-iedit-state)))
