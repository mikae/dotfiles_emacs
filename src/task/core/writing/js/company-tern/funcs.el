;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/tern/activate ()
  "Activate tern in current buffer."
  (tern-mode +1))

;; Init
(defun init ()
  "Configure tern."
  (serika-c/eg/add-install :type         'package
                           :name         'company-tern
                           :package-list '(company-tern)
                           :parents      '("install js2"))

  (serika-c/eg/add-many 'js2
                        ("keymap js2")
                        (func/func/requirer company-tern)))