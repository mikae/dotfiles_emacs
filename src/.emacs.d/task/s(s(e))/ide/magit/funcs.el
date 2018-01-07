;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `magit'."
  (serika-c/eg/add-install :type       'git
                           :name       'magit
                           :src        "https://github.com/shinkiley/magit"
                           :extra-path '("lisp"))

  (dolist (--package '((ghub        . "https://github.com/shinkiley/ghub")
                       (magit-popup . "https://github.com/shinkiley/magit-popup")
                       (with-editor . "https://github.com/shinkiley/with-editor")))
    (serika-c/eg/add-install :type       'git
                             :name       (car --package)
                             :src        (cdr --package)))

  (serika-c/eg/add-many-by-name 'magit
    ("require")
    (func/func/require 'magit)

    ("global-keymap")
    (func/keymap/define-global "C-x m i" 'magit-init
      "C-x m s" 'magit-status
      "C-x m c" 'magit-clone)

    ("hook")
    (progn
      ;; (func/hook/add 'magit-log-mode-hook
      ;;                (serika-f/emojify/create-activator :emoji-styles '(github)))
      ;; (func/hook/add 'magit-status-mode-hook
      ;;                (serika-f/emojify/create-activator :emoji-styles '(github)))
      )))
