;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `pomidor'."
  (serika-c/eg/add-install :type 'git
                           :name 'pomidor
                           :src  "https://github.com/TatriX/pomidor")

  (serika-c/eg/add-many-by-name 'pomidor
    ("require")
    (func/func/require 'pomidor)

    ("settings")
    (setq pomidor-sound-tick     nil
          pomidor-sound-tack     nil
          pomidor-sound-overwork t
          pomidor-seconds        2250)

    ("keymap")
    (progn
      (func/keymap/save   pomidor-mode-map)
      (func/keymap/create pomidor-mode-map
        "n" #'pomidor-stop
        "b" #'pomidor-break
        "r" #'pomidor-reset
        "q" #'quit-window
        "s" #'pomidor-quit))

    ("global-keymap")
    (func/keymap/define-global
      "<C-m> p" #'pomidor)))
