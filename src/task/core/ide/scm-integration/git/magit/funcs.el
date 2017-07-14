;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/magit//require ()
  "Require modules for `magit'."
  (require 'magit))

(defun serika/magit//status-mode-keymap ()
  "Configure `magit-status-mode-map'."
  (setq magit-status-mode-map (make-sparse-keymap))
  (define-key magit-status-mode-map (kbd "s") 'magit-stage-file)
  (define-key magit-status-mode-map (kbd "c") 'magit-commit-popup)
  )

(defun serika/magit//popup-mode-map ()
  "Configure `magit-popup-mode-map'."
  (setq magit-popup-mode-map (make-sparse-keymap))
  (define-key magit-popup-mode-map (kbd "-") 'magit-invoke-popup-switch)
  (define-key magit-popup-mode-map (kbd "=") 'magit-invoke-popup-option)
  )

(defun serika/magit//global-keymap ()
  "Configure global keymap."
  (global-set-key (kbd "C-x m i") 'magit-init)
  (global-set-key (kbd "C-x m s") 'magit-status))

(defun init ()
  "Configure `magit'."
  ;; (serika/magit//status-mode-keymap)
  ;; (serika/magit//popup-mode-map)
  (serika/magit//global-keymap))
