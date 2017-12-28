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
    (progn
      (require 'dashboard))

    ("settings")
    (progn
      (dashboard-setup-startup-hook)
      (setq dashboard-banner-logo-title "Hello :3")
      ;; (setq dashboard-startup-banner    (f-join serika-images-directory
      ;;                                           "greetings.png"))
      (setq dashboard-startup-banner nil)
      (setq dashboard-items '((recents . 5))))

    ("keymap")
    (progn
      (func/keymap/save dashboard-mode-map)
      (func/keymap/define dashboard-mode-map
                          "i" #'widget-backward
                          "e" #'widget-forward
                          "o" #'widget-button-press

                          "I" #'dashboard-previous-section
                          "E" #'dashboard-next-section))))
