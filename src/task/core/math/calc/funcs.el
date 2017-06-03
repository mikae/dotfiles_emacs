;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/calc/keymap ()
  "Configure `calc-mode-map'."
  (setq calc-mode-map (make-sparse-keymap)))

(defun serika/calc/global-keymap ()
  "Add global bindings to invoke calc-mode."
  (global-set-key (kbd "<C-m> c a l c") 'calc))
