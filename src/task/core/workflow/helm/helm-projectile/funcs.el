;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `helm-projectile'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(helm-projectile)
                           :name         'helm-projectile)

  (serika-c/eg/add-many 'helm-projectile
                        ("require")
                        (lambda ()
                          (require 'helm-projectile))

                        ("keymap projectile")
                        (lambda ()
                          (func/keymap/define projectile-mode-map
                                              "C-p h" 'helm-projectile
                                              "C-p f" 'helm-projectile-find-file-dwim
                                              "C-p g" 'helm-projectile-find-file
                                              "C-p b" 'helm-projectile-switch-to-buffer
                                              "C-p o" 'helm-projectile-find-other-file)
                          )))
