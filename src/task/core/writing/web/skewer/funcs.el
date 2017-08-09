;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/skewer/activate ()
  "Activate `skewer-mode'."
  (skewer-mode)

  (func/keymap/save skewer-mode-map)
  (func/keymap/create skewer-mode-map))

(defun serika-f/skewer/activate-css ()
  "Activate `skewer-css-mode'."
  (skewer-css-mode)

  (func/keymap/save skewer-css-mode-map)
  (func/keymap/create skewer-css-mode-map))

(defun serika-f/skewer/activate-html ()
  "Activate `skewer-html-mode'."
  (skewer-html-mode)

  (func/keymap/save skewer-html-mode-map)
  (func/keymap/create skewer-html-mode-map))

;; Init
(defun init ()
  "Configure `skewer-mode'."
  (serika-c/eg/add-install :package-list '(skewer-mode)
                           :name         'skewer))
