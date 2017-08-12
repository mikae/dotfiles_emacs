;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions
;; `prettify-symbols'
;; todo: serika-f/smartparens/load
;; make generic
(defun serika-f/prettify-symbols/create-loader (name)
  "Create lambda that activates `prettify-symbols-mode', and
setups `prettify-symbols-alist'."
  (let* ((--conf (f-read-text (func/path/join serika-conf-directory
                                                                   "prettify-symbols"
                                                                   (concat name
                                                                           ".prettify-symbols"))))
         (--parts (split-string --conf)))
    (when (cl-oddp (length --parts))
      (error "Incorrect prettify configuration file."))
    (lambda ()
      (cl-loop for --left  in --parts       by #'cddr
               for --right in (cdr --parts) by #'cddr
               do
               (push (cons --left --right)
                     prettify-symbols-alist))
      (prettify-symbols-mode +1))))

;; `settings'
(defmacro serika-f/settings/create-configurator (&rest args)
  "Create lambda that setups configuration variables."
  `(if (cl-oddp (length ',args))
       (error "Length of args should be even.")
     (lambda ()
       (let ((--args ',args))
         (cl-loop for --prop  in --args       by #'cddr
                  for --value in (cdr --args) by #'cddr
                  do
                  (set --prop --value))))))

;; `trailing-whitespaces'
(defun serika-f/settings/show-trailing-whitespaces (&optional turn-on)
  "Show trailing whitespaces in current buffer."
  (setq show-trailing-whitespace t))

;; `auto-revert-mode'
(defun serika-f/settings/auto-revert-mode (&optional turn-on)
  "Show trailing whitespaces in current buffer."
  (auto-revert-mode (if turn-on
                        +1
                      -1)))

;; Init
(defun init ()
  "Configure Emacs settings."
  (serika-c/eg/add-many 'settings
                        ("base configure")
                        (lambda ()
                          (setq gc-cons-threshold 50000000)
                          (setq large-file-warning-threshold 100000000)

                          ;; Disable backup
                          (setq make-backup-files        nil
                                auto-save-list-file-name nil
                                auto-save-default        nil)

                          (setq auto-save-list-file-prefix
                                (func/path/join serika-tmp-directory
                                                    "auto-save-list"
                                                    ".saves-"))

                          ;; Disable all default mode selection.
                          (setq auto-mode-alist ())

                          ;; Disable verbose messages of auto revert mode
                          (setq auto-revert-verbose nil)

                          ;; Use spaces instead of tabs
                          (setq-default indent-tabs-mode nil
                                        tab-width        4)

                          (setq word-wrap t)

                          (electric-pair-mode -1)
                          (auto-revert-mode   -1))))
