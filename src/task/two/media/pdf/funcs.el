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
                                                      "q"     #'func/buffer/kill-current

                                                      ;; Move
                                                      "A-e"   #'pdf-view-next-line-or-next-page
                                                      "A-i"   #'pdf-view-previous-line-or-previous-page
                                                      "A-E"   #'pdf-view-scroll-up-or-next-page
                                                      "A-I"   #'pdf-view-scroll-down-or-previous-page
                                                      "A-p"   #'pdf-view-next-page
                                                      "A-P"   #'pdf-view-previous-page
                                                      "A-t"   #'pdf-view-first-page
                                                      "A-T"   #'pdf-view-last-page

                                                      "C-c p" #'pdf-view-goto-page
                                                      "C-c l" #'pdf-view-goto-label

                                                      ;; Scale
                                                      "-"     #'pdf-view-shrink
                                                      "+"     #'pdf-view-enlarge
                                                      "0"     #'pdf-view-scale-reset))))
