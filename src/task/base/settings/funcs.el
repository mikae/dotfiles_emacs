;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/settings//require ()
  "Require modules."
  (require 'func-path))

(defun serika-g/settings//configure ()
  "Configure Emacs settings."
  ;; Configure garbage collector
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
  (auto-revert-mode -1))

(defun init ()
  "Configure Emacs settings."
  (serika-c/eg/add :parents '("base require")
                   :name 'settings
                   :func #'serika-g/settings//require)

  (serika-c/eg/add :parents '("base configure")
                   :name #'settings
                   :func #'serika-g/settings//require))
