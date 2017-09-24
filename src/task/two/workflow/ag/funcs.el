;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `ag'."
  (serika-c/eg/add-install :name         'ag
                           :type         'package
                           :package-list '(ag))

  (serika-c/eg/add-many-by-name 'ag
                                ("require")
                                (lambda ()
                                  (require 'ag))

                                ("settings")
                                (lambda ()
                                  (setq ag-highlight-search t
                                        ag-reuse-window     t
                                        ag-reuse-buffers    t

                                        ag-arguments        ag-arguments
                                        ag-executable       (executable-find "ag")))

                                ("global-keymap")
                                (lambda ()
                                  (serika-f/which-key/define-global-keys
                                   "C-f a"     #'ag        "Find by name"
                                   "C-f A"     #'ag-regexp "Find by regexp"))))
