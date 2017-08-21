;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `zenburn'."
  (serika-c/eg/add-install :type 'package
                           :name 'zenburn
                           :package-list '(zenburn-theme)))
