;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Vars
(defvar --serika-prettify-symbols-cache (make-hash-table :test 'equal)
  "Cache for `prettify-symbols' content files.")

;; Functions
(cl-defun serika-f/prettify-symbols/activate (&key ((:name   --name)   "" --name-p)
                                                   ((:cached --cached) t))
  "Load prettify-symbols file `name'. Cached in `--serika-prettify-symbols-cache'."
  (if --name-p
      (let ((--result))
        (setq --result (or (when --cached
                             (gethash --name --serika-prettify-symbols-cache))
                           (let* ((--filepath (f-join serika-conf-directory
                                                      "prettify-symbols"
                                                      (concat --name
                                                              ".prettify-symbols")))
                                  (--parts (split-string (f-read-text --filepath))))
                             (when (cl-oddp (length --parts))
                               (error "Incorrect `prettify-symbols' configuration file."))
                             (when --cached
                               (puthash --name --parts --serika-prettify-symbols-cache))
                             --parts)))
        (when (> (length --result)
                 0)
          (setq prettify-symbols-alist ())
          (cl-loop for --first  in --result       by #'cddr
                   for --second in (cdr --result) by #'cddr
                   do
                   (push (cons --first --second) prettify-symbols-alist))
          (prettify-symbols-mode +1)))
    (error "Name was not provided.")))

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

(defun serika-f/settings/register-ft (pattern mode)
  "Add new item in `auto-mode-alist'."
  (add-to-list 'auto-mode-alist `(,pattern . ,mode)))

(defun serika-f/settings/change-user ()
  "Change user settings."
  (interactive)
  (let ((--user-name (read-string "Enter user name: " user-full-name))
        (--user-mail (read-string "Enter user f-mail: " user-mail-address)))
    (when (stringp --user-name)
      (setq user-full-name --user-name))
    (when (stringp --user-mail)
      (setq user-mail-address --user-mail))))

;; `trailing-whitespaces'
(defun serika-f/settings/show-trailing-whitespaces (&optional turn-on)
  "Show/disable trailing whitespaces in current buffer."
  (setq show-trailing-whitespace (or turn-on t)))

;; `auto-revert-mode'
(defun serika-f/settings/auto-revert-mode (&optional turn-on)
  "Show trailing whitespaces in current buffer."
  (auto-revert-mode (if turn-on
                        +1
                      -1)))

;; Init
(defun init ()
  "Configure Emacs settings."
  (serika-c/eg/add-many-by-name 'settings
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
                                  (electric-indent-mode -1)
                                  (auto-revert-mode   -1))))
