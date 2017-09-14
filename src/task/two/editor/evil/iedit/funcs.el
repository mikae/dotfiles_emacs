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
                                (lambda ()
                                  (require 'iedit))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save   iedit-mode-keymap)
                                  (func/keymap/create iedit-mode-keymap)

                                  (func/keymap/save   iedit-mode-occurrence-keymap)
                                  (func/keymap/create iedit-mode-occurrence-keymap))

                                ("settings evil")
                                (lambda ()
                                  (evil-define-state iedit
                                    "Iedit state"
                                    :tag "<IE>"
                                    :suppress-keymap t))

                                ("keymap evil")
                                (lambda ()
                                  (func/keymap/define evil-iedit-state-map
                                                      ;; qwfwp
                                                      "q" #'iedit-mode
                                                      "Q" #'evil-iedit-input-state
                                                      "w" #'iedit-prev-occurrence
                                                      "W" #'iedit-next-occurrence
                                                      "f" #'iedit-toggle-selection

                                                      "C-, C-n" #'evil-normal-state))

                                ("keymap evil normal")
                                (lambda ()
                                  (func/keymap/define evil-normal-state-map
                                                      "l" #'evil-iedit-state))))
