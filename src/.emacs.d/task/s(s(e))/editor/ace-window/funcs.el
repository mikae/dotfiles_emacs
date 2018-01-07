;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure ace-window."
  (serika-c/eg/add-install :type 'git
                           :name 'ace-window
                           :src  "https://github.com/shinkiley/ace-window")

  (serika-c/eg/add-many-by-name 'ace-window
    ("require")
    (func/func/require 'ace-window)

    ("settings")
    (setq aw-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)
          aw-dispatch-always t)))
