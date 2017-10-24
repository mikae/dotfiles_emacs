;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `doom-themes'."
  (serika-c/eg/add-install :type 'package
                           :name 'doom-themes
                           :package-list '(doom-themes))

  (serika-c/eg/add-many-by-name 'theme
                                ("require")
                                (func/func/require 'doom-themes)

                                ("interface")
                                (lambda ()
                                  (load-theme 'doom-one t))

                                ;; ("settings org")
                                ;; (lambda ()
                                ;;   (doom-themes-org-config))
                                ))
