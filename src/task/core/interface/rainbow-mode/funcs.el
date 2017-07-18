;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika-g/rainbow-mode//require ()
  "Require modules for `rainbow-mode'."
  (require 'rainbow-mode))

(defun serika-g/rainbow-mode//configure ()
  "Configure `rainbow-mode'."
  nil)

(defun init ()
  "Configure `rainbow-mode'."
  (serika-c/eg/add-install :package-list '(rainbow-mode)
                           :name         'rainbow-mode)

  (serika-c/eg/add :parents '("require")
                   :name    'rainbow-mode
                   :func    #'serika-g/rainbow-mode//require)

  (serika-c/eg/add :parents '("interface")
                   :name    'rainbow-mode
                   :func    #'serika-g/rainbow-mode//configure))
