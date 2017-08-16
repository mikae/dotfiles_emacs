;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `ibuffer'."
  (serika-c/eg/add-many 'ibuffer
                        ("require")
                        (lambda ()
                          (require 'ibuffer)
                          (require 'ibuf-ext))

                        ("settings")
                        (lambda ()
                          (add-to-list 'ibuffer-never-show-predicates "^\\*")
                          (setq ibuffer-saved-filter-groups nil))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   ibuffer-mode-map)
                          (func/keymap/create ibuffer-mode-map
                                              "A-e"        #'ibuffer-forward-line
                                              "A-i"        #'ibuffer-backward-line
                                              "A-E"        #'ibuffer-forward-filter-group
                                              "A-I"        #'ibuffer-backward-filter-group

                                              "d d"        #'ibuffer-do-delete
                                              "d s"        #'ibuffer-do-save
                                              "d f"        #'ibuffer-find-file

                                              "s a"        #'ibuffer-do-sort-by-alphabetic
                                              "s f"        #'ibuffer-do-sort-by-filename/process
                                              "s i"        #'ibuffer-invert-sorting
                                              "s m"        #'ibuffer-do-sort-by-major-mode
                                              "s s"        #'ibuffer-do-sort-by-size

                                              "* *"        #'ibuffer-unmark-all
                                              "* m"        #'ibuffer-mark-by-mode
                                              "* c"        #'ibuffer-mark-unsaved-buffers
                                              "* h"        #'ibuffer-mark-help-buffers
                                              "* d"        #'ibuffer-mark-dired-buffers
                                              "* r"        #'ibuffer-mark-read-only-buffers
                                              "* r"        #'ibuffer-mark-read-only-buffers

                                              "RET"        #'ibuffer-visit-buffer
                                              "<C-return>" #'ibuffer-visit-buffer-other-window)
                          (func/keymap/bind-digits ibuffer-mode-map #'digit-argument))))
