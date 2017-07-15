;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/company//require ()
  "Require modules for `company'."
  (require 'func-package)
  (require 'company))

(defun serika-g/company//settings ()
  "Configure `company-mode'."
  (setq company-minimum-prefix-length 2)
  (setq company-mode/enable-yas t))

(defun serika-g/company//keymap ()
  "Configure `company-active-map'."
  (setq company-active-map (make-sparse-keymap))

  (let ((map company-active-map))
    (define-key company-active-map (kbd "A-l") 'company-complete-selection)
    (define-key company-active-map (kbd "A-k") 'company-select-previous)
    (define-key company-active-map (kbd "A-j") 'company-select-next)
    (define-key company-active-map (kbd "A-h") 'company-abort)))

(defun init ()
  "Configure `company'."
  (serika-g/company//require)
  (serika-g/company//settings)
  (serika-g/company//keymap))
