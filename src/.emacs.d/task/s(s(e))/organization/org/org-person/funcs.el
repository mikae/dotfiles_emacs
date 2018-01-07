;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `org-person'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-person
                           :src  "https://github.com/shinkiley/org-person")

  (serika-c/eg/add-many-by-name 'org-person
    ("require org")
    (func/func/require 'org-person)

    ("settings org")
    (progn
      (setq org-person-directory (f-join org-directory
                                         "Person")))))
