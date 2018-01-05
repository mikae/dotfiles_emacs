;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/beacon/activate ()
  "Activate beacon mode"
  (unless beacon-mode
    (beacon-mode +1)))

(defun init ()
  "Configure `beacon'."
  (serika-c/eg/add-install :type    'git
                           :name    'beacon
                           :src     "https://github.com/shinkiley/beacon")

  (serika-c/eg/add-many-by-name 'beacon
    ("require")
    (func/func/require 'beacon)

    ("settings")
    (setq beacon-color                               "#917b55"
          beacon-blink-when-window-changes           t
          beacon-blink-when-window-scrolls           nil
          beacon-blink-when-point-moves-horizontally nil
          beacon-blink-when-point-moves-vertically   nil)

    ("post activate")
    (serika-f/beacon/activate)))
