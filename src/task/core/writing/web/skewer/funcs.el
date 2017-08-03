;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/skewer/activate ()
  "Activate `skewer-mode'."
  (skewer-mode)

  (setq --serika-skewer-mode-map skewer-mode-map)
  (serika-f/keymap/create skewer-mode-map))

(defun serika-f/skewer/activate-css ()
  "Activate `skewer-css-mode'."
  (skewer-css-mode)

  (setq --serika-skewer-css-mode-map skewer-css-mode-map)
  (serika-f/keymap/create skewer-css-mode-map))

(defun serika-f/skewer/activate-html ()
  "Activate `skewer-html-mode'."
  (skewer-html-mode)

  (setq --serika-skewer-html-mode-map skewer-html-mode-map)
  (serika-f/keymap/create skewer-html-mode-map))

;; Init
(defun init ()
  "Configure `skewer-mode'."
  (serika-c/eg/add-install :package-list '(skewer-mode)
                           :name         'skewer))
