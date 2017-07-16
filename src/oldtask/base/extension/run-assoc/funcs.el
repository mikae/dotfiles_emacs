;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/run-assoc//require ()
  "Require modules for `run-assoc'."
  (require 'run-assoc))

(defun serika-g/run-assoc//variables ()
  "Configure `run-assoc' variables."
  (setq associated-program-alist
        '(("animate" "\\.gif$")
          ("smplayer" "\\.webm$"))))

(defun init ()
  "Configure `run-assoc'."
  (serika-g/run-assoc//require)
  (serika-g/run-assoc//variables))
