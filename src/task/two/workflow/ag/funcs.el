;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `ag'."
  (serika-c/eg/add-install :type    'git
                           :name    'ag
                           :src     "https://github.com/shinkiley/ag.el")

  (serika-c/eg/add-many-by-name 'ag
    ("require")
    (func/func/require 'ag)

    ("settings")
    (setq ag-highlight-search t
          ag-reuse-window     t
          ag-reuse-buffers    t

          ag-arguments        ag-arguments
          ag-executable       (executable-find "ag"))

    ("global-keymap")
    (serika-f/which-key/define-global-keys
     "C-f a" #'ag        "Find by name"
     "C-f A" #'ag-regexp "Find by regexp")))
