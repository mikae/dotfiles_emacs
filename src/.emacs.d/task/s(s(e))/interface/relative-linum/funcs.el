;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/linum-relative/activate ()
  "Activate relative linum."
  (linum-on)
  (linum-relative-on))

;; Init
(defun init ()
  "Configure `linum'."
  (serika-c/eg/add-install :type    'git
                           :name    'linum-relative
                           :src     "https://github.com/shinkiley/linum-relative")

  (serika-c/eg/add-many-by-name 'linum-relative
    ("require")
    (func/func/require 'linum-relative)

    ("settings")
    (setq linum-relative-current-symbol "")))
