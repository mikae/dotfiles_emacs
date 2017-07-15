;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/bs//require ()
  "Require modules for `bs'."
  (require 'bs))

(defun serika-g/bs//settings ()
  "Configure `bs'."
  (setq bs-configurations
        '(("files" "^*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))))

(defun serika-g/bs//keymap ()
  "Configure `bs' keymap."
  (setq bs-mode-map (make-sparse-keymap))

  (define-key bs-mode-map (kbd "q")     'bs-abort)
  (define-key bs-mode-map (kbd "d")     'bs-delete)
  (define-key bs-mode-map (kbd "s")     'bs-save)
  (define-key bs-mode-map (kbd "RET")   'bs-select)
  (define-key bs-mode-map (kbd "j")     'next-line)
  (define-key bs-mode-map (kbd "k")     'previous-line)
  (define-key bs-mode-map (kbd "l")     'bs-select))

(defun serika-g/bs//global-keymap ()
  "Configure global keymap to use `bs'."
  (global-set-key (kbd "C-, b l")       'bs-show))

(defun init ()
  "Configure `bs'."
  (serika-g/bs//require)
  (serika-g/bs//settings)
  (serika-g/bs//keymap)
  (serika-g/bs//global-keymap))
