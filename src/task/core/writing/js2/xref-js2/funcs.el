;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `xref-js2'."
  (serika-c/eg/add-install :type 'package
                           :name 'xref-js2
                           :package-list '(xref-js2)
                           :parents '("install js2"))

  (serika-c/eg/add-many-by-name 'xref-js2
                                ("require js2")
                                (func/func/requirer 'xref-js2))
  )
