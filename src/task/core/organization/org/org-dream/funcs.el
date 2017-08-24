;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `org-dream'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-dream
                           :src  "https://github.com/mikae/org-dream")
  (serika-c/eg/add-many-by-name 'org-dream
                                ("require org")
                                (func/func/requirer org-dream)

                                ("settings org")
                                (lambda ()
                                  (setq org-dream-location (f-join org-directory
                                                                   "Dream"))
                                  )))
