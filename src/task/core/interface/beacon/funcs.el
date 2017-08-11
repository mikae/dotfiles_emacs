;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `beacon'."
  (serika-c/eg/add-install :type 'package
                           :name 'beacon
                           :package-list '(beacon))
  (serika-c/eg/add-many 'beacon
                        ("require")
                        (lambda ()
                          (require 'beacon))

                        ("settings")
                        (lambda ()
                          (setq beacon-color "#917b55")
                          (setq beacon-blink-when-window-changes           t
                                beacon-blink-when-window-scrolls           nil
                                beacon-blink-when-point-moves-horizontally nil
                                beacon-blink-when-point-moves-vertically   nil))))
