;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-path)

(defun serika/settings//configure ()
  "Configure Emacs settings."
  ;; Configure garbage collector
  (setq gc-cons-threshold 50000000)
  (setq large-file-warning-threshold 100000000)

  ;; Disable backup
  (setq make-backup-files        nil
        auto-save-list-file-name nil
        auto-save-default        nil)

  (setq auto-save-list-file-prefix
        (serika/path/join serika-tmp-directory
                          "auto-save-list"
                          ".saves-"))

  ;; Use spaces instead of tabs
  (setq-default indent-tabs-mode nil
                tab-width        4)

  (setq word-wrap t)

  (electric-pair-mode -1)
  (auto-revert-mode -1))

(defun init ()
  "Configure Emacs settings."
  (serika/settings//configure))
