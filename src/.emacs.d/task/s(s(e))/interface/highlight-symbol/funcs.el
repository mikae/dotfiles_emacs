;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/highlight-symbol/activate ()
  "Activate `highlight-symbol' in current buffer."
  (highlight-symbol-mode +1))

;; Init
(defun init ()
  "Configure `highlight-symbol'."
  (serika-c/eg/add-install :type    'git
                           :name    'highlight-symbol
                           :src     "https://github.com/shinkiley/highlight-symbol.el")

  (serika-c/eg/add-many-by-name 'highlight-symbol
    ("require")
    (func/func/require 'highlight-symbol)

    ("settings")
    (setq highlight-symbol-idle-delay 0.1)))
