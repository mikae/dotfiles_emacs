;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/js//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(defun init ()
  "Configure Emacs for `js'-programming."
  (serika/js//auto-mode-alist)
  )
