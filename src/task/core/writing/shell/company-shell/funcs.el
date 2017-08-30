;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `company-shell'."
  (serika-c/eg/add-install :type 'package
                           :name 'company-shell
                           :package-list '(company-shell))
  (serika-c/eg/add-many-by-name 'company-eshell
                                ("require")
                                (func/func/requirer 'company-shell))
  )
