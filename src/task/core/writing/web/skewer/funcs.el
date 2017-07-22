;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/skewer/activate ()
  "Activate `skewer-mode'."
  (skewer-mode))

(defun serika-f/skewer/activate-css ()
  "Activate `skewer-css-mode'."
  (skewer-css-mode)
  (setq --serika-skewer-css-mode-map skewer-css-mode-map)
  (setq skewer-css-mode-map (let ((map (make-sparse-keymap)))
                         map)))

(defun serika-f/skewer/activate-html ()
  "Activate `skewer-html-mode'."
  (skewer-html-mode)
  (setq --serika-skewer-html-mode-map skewer-html-mode-map)
  (setq skewer-html-mode-map (let ((map (make-sparse-keymap)))
                         map)))

;; Global
(defun serika-g/skewer//keymap ()
  "Configure `skewer-mode-map'."
  ;; `js2'
  (setq --serika-skewer-mode-map skewer-mode-map)
  (setq skewer-mode-map (let ((map (make-sparse-keymap)))
                          map)))

(defun init ()
  "Configure `skewer-mode'."
  (serika-c/eg/add-install :package-list '(skewer-mode)
                           :name         'skewer)

  (serika-c/eg/add :parents '("keymap")
                   :name    'skewer
                   :func    #'serika-g/skewer//keymap))
