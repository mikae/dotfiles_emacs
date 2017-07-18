;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/rainbow-delimiters//require ()
  "Require modules for `rainbow-delimiters'."
  (require 'rainbow-delimiters))

(defun serika-g/rainbow-delimiters//configure ()
  "Configure `rainbow-delimiters'."
  nil)

;; Init
(defun init ()
  "Configure `rainbow-delimiters'."
  (serika-c/eg/add-install :package-list '(rainbow-delimiters)
                           :name         'rainbow-delimiters)

  (serika-c/eg/add :parents '("require")
                   :name    'rainbow-delimiters
                   :func    #'serika-g/rainbow-delimiters//require)

  (serika-c/eg/add :parents '("interface")
                   :name    'rainbow-delimiters
                   :func    #'serika-g/rainbow-delimiters//configure))
