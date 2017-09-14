;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `org-person'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-person
                           :src  "https://github.com/mikae/org-person")
  (serika-c/eg/add-many-by-name 'org-person
                                ("require org")
                                (lambda ()
                                  (require 'org-person))

                                ("settings org")
                                (lambda ()
                                  (setq org-person-directory
                                        (f-join org-directory
                                                "Person")))))
