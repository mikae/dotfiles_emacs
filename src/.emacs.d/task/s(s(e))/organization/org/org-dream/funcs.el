;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `org-dream'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-dream
                           :src  "https://github.com/shinkiley/org-dream")

  (serika-c/eg/add-many-by-name 'org-dream
    ("require org")
    (func/func/require 'org-dream)

    ("settings org")
    (progn
      (org-dream-set-home (f-join org-directory
                                  "Dream")))))
