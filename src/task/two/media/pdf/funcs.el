;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `pdf'."
  (serika-c/eg/add-install :package-list '(pdf-tools)
                           :name         'pdf)

  (serika-c/eg/add-many-by-name 'pdf
                                ("require")
                                (lambda ()
                                  (require 'pdf-tools)
                                  (pdf-tools-install))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'pdf-view-mode "\\.pdf\\'"))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save   pdf-view-mode-map)
                                  (func/keymap/create pdf-view-mode-map
                                                      ;; qwfpg
                                                      "q" #'func/buffer/kill-current

                                                      ;; arstd
                                                      "a" #'pdf-view-next-page
                                                      "A" #'pdf-view-previous-page
                                                      "r" #'pdf-view-first-page
                                                      "R" #'pdf-view-last-page
                                                      "s" #'pdf-view-goto-page
                                                      "S" #'pdf-view-goto-label

                                                      ;; neio
                                                      "e" #'pdf-view-next-line-or-next-page
                                                      "i" #'pdf-view-previous-line-or-previous-page
                                                      "E" #'pdf-view-scroll-up-or-next-page
                                                      "I" #'pdf-view-scroll-down-or-previous-page

                                                      ;; Scale
                                                      "-" #'pdf-view-shrink
                                                      "+" #'pdf-view-enlarge
                                                      "0" #'pdf-view-scale-reset)

                                  (func/keymap/save pdf-misc-minor-mode-map)
                                  (func/keymap/create pdf-misc-minor-mode-map))))
