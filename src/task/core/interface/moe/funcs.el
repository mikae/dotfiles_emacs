;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/moe-theme/require ()
  "Require modules for `moe'."
  (require 'func-package)
  (require 'moe-theme))

(defun serika-g/moe-theme/configure ()
  "Configure `moe-theme'."
  (setq moe-theme-highlight-buffer-id t)

  (moe-theme-random-color)
  (powerline-moe-theme)
  (moe-dark))

;; Init
(defun init ()
  "Configure `moe-theme'."
  (serika-c/eg/add-install :package-list '(moe-theme)
                           :name         'moe-theme
                           :parents      '("base interface install powerline configure"))

  (serika-c/eg/add :parents '("base interface install powerline configure moe-theme")
                   :name    'require
                   :func    #'serika-g/moe-theme/require)

  (serika-c/eg/add :parents '("base interface install powerline configure moe-theme")
                   :name    'configure
                   :func    #'serika-g/moe-theme/configure))
