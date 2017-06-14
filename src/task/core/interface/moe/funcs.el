;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)
(require 'moe-theme)

(defun init ()
  "Configure `moe-theme'."
  (setq moe-theme-highlight-buffer-id t)

  (moe-theme-random-color)
  (powerline-moe-theme)
  (moe-dark))
