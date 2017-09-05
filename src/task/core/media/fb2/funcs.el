;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure fb2"
  (serika-c/eg/add-install :type 'git
                           :name 'fb2-mode
                           :src  "https://github.com/mikae/fb2-mode")

  (serika-c/eg/add-many-by-name 'fb2-mode
                                ("require")
                                ;; (func/func/requirer 'fb2-mode)
                                (lambda ()
                                  (require 'fb2-mode))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'fb2-mode "\\.fb2\\'"))))
