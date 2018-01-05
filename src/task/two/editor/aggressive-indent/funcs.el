;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/aggressive-indent/activate ()
  "Activate `aggressive-indent' in current buffer."
  (aggressive-indent-mode +1))

(defun serika-f/aggressive-indent/toggle ()
  "Toggle agressive indent."
  (interactive)
  (aggressive-indent-mode (if aggressive-indent-mode
                              -1
                            +1)))

;; Init
(defun init ()
  "Configure `aggressive-indent'."
  (serika-c/eg/add-install :type 'git
                           :name 'aggressive-indent
                           :src  "https://github.com/shinkiley/aggressive-indent-mode")

  (serika-c/eg/add-many-by-name 'aggressive-indent
    ("require")
    (func/func/require 'aggressive-indent)

    ("settings")
    (setq aggressive-indent-dont-indent-if
          (list 'custom-modifiers-mode))

    ("global-keymap")
    (func/keymap/define-global
      "C-x t a" #'serika-f/aggressive-indent/toggle)))
