;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `evil-visualstar'."
  (serika-c/eg/add-install :type 'git
                           :name 'evil-visualstar
                           :src  "https://github.com/mikae/evil-visualstar"
                           :parents '("install evil"))

  (serika-c/eg/add-many-by-name 'evil-visualstar
                                ("require evil")
                                (func/func/requirer 'evil-visualstar)

                                ("keymap evil visual")
                                (lambda ()
                                  (func/keymap/define evil-visual-state-map
                                                      "A-1" #'evil-visualstar/begin-search-forward
                                                      "A-!" #'evil-visualstar/begin-search-forward)))
  )
