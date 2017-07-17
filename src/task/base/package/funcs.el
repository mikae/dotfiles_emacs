;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/package//require ()
  "Require modules."
  (require 'func-package)
  (message "require"))

(defun serika-g/package//configure ()
  "Configure package manager."
  (serika-f/package/repository-clear)

  (serika-f/package/repository-add "gnu"          "http://elpa.gnu.org/packages/")
  (serika-f/package/repository-add "melpa-stable" "https://stable.melpa.org/packages/")
  (serika-f/package/repository-add "melpa"        "http://melpa.org/packages/")

  (serika-f/package/initialize)
  (serika-f/package/list-update)
  (message "configure"))

;; Init
(defun init ()
  (message "hoi")
  (serika-c/eg/add :parents '("base require")
                   :name    'package-manager
                   :func    #'serika-g/package//require)

  (serika-c/eg/add :parents '("base configure")
                   :name    'package-manager
                   :func    #'serika-g/package//configure))
