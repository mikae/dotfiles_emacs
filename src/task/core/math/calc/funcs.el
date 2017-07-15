;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/calc/keymap ()
  "Configure `calc-mode-map'."
  ;; (setq calc-mode-map (make-sparse-keymap))
  )

(defun serika-g/calc/global-keymap ()
  "Add global bindings to invoke calc-mode."
  (global-set-key (kbd "<C-m> c a l c") 'calc))

(defun init ()
  "Configure `calc-mode'."
  (serika-g/calc/keymap)
  (serika-g/calc/global-keymap))
