;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `ag'."
  (serika-c/eg/add-install :name         'ag
                           :type         'package
                           :package-list '(ag))

  (serika-c/eg/add-many 'ag
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
                          (func/keymap/define-global
                           "C-f a f"     #'ag-files
                           "C-f a r"     #'ag-regexp
                           "C-f a a"     #'ag

                           "C-f a d d"   #'ag-dired
                           "C-f a d r"   #'ag-dired-regexp

                           "C-f a p p"   #'ag-project
                           "C-f a p f"   #'ag-project-files
                           "C-f a p r"   #'ag-project-regexp
                           "C-f a p a"   #'ag-project-at-point

                           "C-f a p d d" #'ag-project-dired
                           "C-f a p d r" #'ag-project-dired-regexp
                           "C-f a p d d" #'ag-project-dired))))
