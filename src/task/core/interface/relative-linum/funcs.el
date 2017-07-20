;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/linum-relative/activate ()
  "Activate relative linum."
  (linum-on)
  (linum-relative-on))

;; Global
(defun serika-g/linum-relative//require ()
  "Require modules for linum"
  (require 'linum-relative))

;; Init
(defun init ()
  "Configure `linum'."
  (serika-c/eg/add-install :package-list '(linum-relative)
                           :name         'linum-relative)

  (serika-c/eg/add :parents '("require")
                   :name    'linum-relative
                   :func    #'serika-g/linum-relative//require))
