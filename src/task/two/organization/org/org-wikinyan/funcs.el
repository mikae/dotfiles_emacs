;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
;; Init
(defun init ()
  "Configure `org-wiki'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-wikinyan
                           :src  "https://github.com/shinkiley/org-wikinyan")

  (serika-c/eg/add-many-by-name 'org-wiki
    ("require org")
    (func/func/require 'org-wikinyan)

    ("settings org")
    (progn
      (setq org-wikinyan-location (f-join org-directory
                                          "wiki"))

      ;; NODE_PATH
      (setenv "NODE_PATH"
              (concat
               (f-join org-wikinyan-location
                       "programming"
                       "javascript"
                       "node_modules")
               ":"
               (getenv "NODE_PATH"))))

    ("keymap org")
    (progn
      ;; (func/keymap/define org-mode-map
      ;;                     "C-c C-z w" #'serika-f/org-wiki/create)
      )))
