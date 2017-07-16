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
  (serika-g/moe-theme/require)
  (serika-g/moe-theme/configure))
