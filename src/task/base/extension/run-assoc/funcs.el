;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'run-assoc)


(defun serika/run-assoc//variables ()
  "Configure `run-assoc' variables."
  (setq associated-program-alist
        '(("animate" "\\.gif$")
          ("smplayer" "\\.webm$"))))
