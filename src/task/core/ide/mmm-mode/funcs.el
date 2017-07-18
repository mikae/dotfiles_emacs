;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/mmm-mode//activate ()
  "Activate `mmm-mode'."
  (mmm-mode +1))

;; Global
(defun serika-g/mmm-mode//require ()
  "Require modules for `mmm-mode'."
  (require 'mmm-mode))

(defun serika-g/mmm-mode//settings ()
  "Clear default `mmm-mode' state."
  nil)

;; Init
(defun init ()
  "Configure `mmm-mode'."
  (serika-c/eg/add-install :package-list '(mmm-mode)
                           :name         'mmm-mode)

  (serika-c/eg/add :parents '("require")
                   :name    'mmm-mode
                   :func    #'serika-g/mmm-mode//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'mmm-mode
                   :func    #'serika-g/mmm-mode//settings))
