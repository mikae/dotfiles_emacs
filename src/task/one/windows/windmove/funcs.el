;;; package --- Summary
;;; Commentary: 
;;; Code: 

(defun init ()
  "Configure `windmove'."
  (serika-c/eg/add-many-by-name 'windmove
                                ("require")
                                (lambda ()
                                  (require 'windmove))))
