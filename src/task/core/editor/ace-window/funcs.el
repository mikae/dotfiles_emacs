;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure ace-window."
  (serika-c/eg/add-install :package-list '(ace-window)
                           :name 'ace-window)

  (serika-c/eg/add-many 'ace-window
                        ("require")
                        (lambda ()
                          (require 'ace-window))

                        ("settings")
                        (lambda ()
                          (setq aw-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
                          (setq aw-dispatch-always t))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "C-w s" #'ace-window))))
