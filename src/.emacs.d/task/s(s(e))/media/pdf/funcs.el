;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `pdf'."
  (serika-c/eg/add-install :type       'git
                           :name       'pdf
                           :src        "https://github.com/shinkiley/pdf-tools"
                           :extra-path '("lisp"))

  (serika-c/eg/add-many-by-name 'pdf
    ("require")
    (progn
      (func/func/require 'pdf-tools
                         'pdf-misc
                         'pdf-occur)
      (pdf-tools-install))

    ("settings")
    (progn
      (serika-f/settings/register-ft 'pdf-view-mode "\\.pdf\\'"))

    ("keymap")
    (progn
      (func/keymap/save   pdf-view-mode-map)
      (func/keymap/create pdf-view-mode-map
        ;; qwfpg
        "q" #'func/buffer/kill-current
        "w" #'pdf-view-shrink
        "W" #'pdf-view-enlarge
        "C-w" #'pdf-view-scale-reset

        ;; arstd
        "a" #'pdf-view-goto-page
        "r" #'pdf-view-goto-label
        "A-t" #'pdf-view-first-page
        "A-T" #'pdf-view-last-page

        ;; neio
        "e" #'pdf-view-next-line-or-next-page
        "i" #'pdf-view-previous-line-or-previous-page
        "E" #'pdf-view-next-page
        "I" #'pdf-view-previous-page)

      (func/keymap/save pdf-misc-minor-mode-map)
      (func/keymap/create pdf-misc-minor-mode-map))))
