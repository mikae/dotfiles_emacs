;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `pomidor'."
  (serika-c/eg/add-install :type 'git
                           :name 'pomidor
                           :src  "https://github.com/TatriX/pomidor")

  (serika-c/eg/add-many 'pomidor
                        ("require")
                        (lambda ()
                          (require 'pomidor))

                        ("settings")
                        (lambda ()
                          (setq pomidor-sound-tick     nil
                                pomidor-sound-tack     nil
                                pomidor-sound-overwork t
                                pomidor-seconds        1500))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   pomidor-mode-map)
                          (func/keymap/create pomidor-mode-map
                                              "n" #'pomidor-stop
                                              "b" #'pomidor-break
                                              "r" #'pomidor-reset
                                              "q" #'quit-window
                                              "s" #'pomidor-quit))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global  "<C-m> p" #'pomidor))))
