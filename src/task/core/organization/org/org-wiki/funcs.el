;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `org-wiki'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-wiki
                           :src  "https://github.com/mikae/org-wiki")

  (serika-c/eg/add-many-by-name 'org-wiki
                                ("require org")
                                (func/func/requirer org-wiki)

                                ("settings org")
                                (lambda ()
                                  (setq org-wiki-location (f-join org-directory
                                                                  "wiki")))))
