;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
;; `prettify-symbols'
(defmacro serika-f/prettify-symbols/create-configurator (&rest args)
  "Create lambda that configures `prettify-symbols'."
  `(progn
     (if (cl-oddp (length ',args))
       (error "Length of args should be even.")
     (lambda ()
       (prettify-symbols-mode +1)
       (setq prettify-symbols-alist ())

       (let ((--args ',args))
         (while --args
           (push (list (car --args) (nth 1 --args)) prettify-symbols-alist)
           (setq --args (nthcdr 2 --args))))))))

;; Init
(defun init ()
  "Configure Emacs settings."
  (serika-c/eg/add-many 'settings
                        ("base require")
                        (lambda ()
                          (require 'func-path))

                        ("base configure")
                        (lambda ()
                          (setq gc-cons-threshold 50000000)
                          (setq large-file-warning-threshold 100000000)

                          ;; Disable backup
                          (setq make-backup-files        nil
                                auto-save-list-file-name nil
                                auto-save-default        nil)

                          (setq auto-save-list-file-prefix
                                (serika-f/path/join serika-tmp-directory
                                                    "auto-save-list"
                                                    ".saves-"))

                          ;; Disable all default mode selection.
                          (setq auto-mode-alist ())

                          ;; Use spaces instead of tabs
                          (setq-default indent-tabs-mode nil
                                        tab-width        4)

                          (setq word-wrap t)

                          (electric-pair-mode -1)
                          (auto-revert-mode -1))))
