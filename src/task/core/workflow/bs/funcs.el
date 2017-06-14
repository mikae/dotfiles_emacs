;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'bs)

(defun serika/bs//settings ()
  "Configure `bs'."
  (setq bs-configurations
        '(("files" "^*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last))))

(defun serika/bs//keymap ()
  "Configure `bs' keymap."
  (setq bs-mode-map (make-sparse-keymap))

  (define-key bs-mode-map (kbd "q")     'bs-abort)
  (define-key bs-mode-map (kbd "d")     'bs-delete)
  (define-key bs-mode-map (kbd "s")     'bs-save)
  (define-key bs-mode-map (kbd "RET")   'bs-select)
  (define-key bs-mode-map (kbd "j")     'next-line)
  (define-key bs-mode-map (kbd "k")     'previous-line)
  (define-key bs-mode-map (kbd "l")     'bs-select))

(defun serika/bs//global-keymap ()
  "Configure global keymap to use `bs'."
  (global-set-key (kbd "C-, b l")       'bs-show))

(defun init ()
  "Configure `bs'."
  (serika/bs//settings)
  (serika/bs//keymap)
  (serika/bs//global-keymap))
