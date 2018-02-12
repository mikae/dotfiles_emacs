;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `buffer-move'."
  (serika-c/eg/add-install :type 'download
                           :name 'buffer-move
                           :src  "https://raw.githubusercontent.com/shinkiley/emacswiki.org/master/buffer-move.el")

  (serika-c/eg/add-many-by-name 'buffer-move
    ("require")
    (func/func/require 'buffer-move)

    ("settings")
    (setq buffer-move-behavior        'move
          buffer-move-stay-after-swap nil)))
