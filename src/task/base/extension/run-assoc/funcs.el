;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'run-assoc)

(defun serika/run-assoc//variables ()
  "Configure `run-assoc' variables."
  (setq associated-program-alist
        '(("animate" "\\.gif$")
          ("smplayer" "\\.webm$"))))

(defun init ()
  "Configure `run-assoc'."
  (serika/run-assoc//variables))
