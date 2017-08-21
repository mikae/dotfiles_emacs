;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `dasboard'."
  (serika-c/eg/add-install :type 'package
                           :name 'dashboard
                           :package-list '(dashboard))
  (serika-c/eg/add-many-by-name 'dashboard
                                ("require")
                                (lambda ()
                                  (require 'dashboard))

                                ("settings")
                                (lambda ()
                                  (dashboard-setup-startup-hook)
                                  (setq dashboard-banner-logo-title "Hello :3")
                                  (setq dashboard-startup-banner    (f-join serika-images-directory
                                                                            "greetings.png"))
                                  (setq dashboard-items '((recents  . 5)
                                                          (bookmarks . 5)
                                                          (projects . 5)
                                                          (agenda . 5))))))
