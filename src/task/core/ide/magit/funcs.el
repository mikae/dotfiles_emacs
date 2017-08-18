;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `magit'."
  (serika-c/eg/add-install :type         'package
                           :package-list '(magit)
                           :name         'magit)

  (serika-c/eg/add-many-by-name 'magit
                                ("require")
                                (lambda ()
                                  (require 'magit))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-x m i" 'magit-init
                                                             "C-x m s" 'magit-status))

                                ("hook")
                                (lambda ()
                                  ;; (func/hook/add 'magit-log-mode-hook
                                  ;;                (serika-f/emojify/create-activator :emoji-styles '(github)))
                                  ;; (func/hook/add 'magit-status-mode-hook
                                  ;;                (serika-f/emojify/create-activator :emoji-styles '(github)))
                                  )))
