;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `doom-themes'."
  (serika-c/eg/add-install :type    'git
                           :name    'doom-themes
                           :src     "https://github.com/shinkiley/emacs-doom-themes")

  (serika-c/eg/add-many-by-name 'theme
    ("require")
    (func/func/require 'doom-themes)

    ("interface")
    (load-theme 'doom-one t)

    ;; ("settings org")
    ;; (lambda ()
    ;;   (doom-themes-org-config))
    ))
