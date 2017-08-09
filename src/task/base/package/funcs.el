;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/package//require ()
  "Require modules."
  (require 'func-package))

(defun serika-g/package//configure ()
  "Configure package manager."
  (serika-f/package/repository-clear)

  (serika-f/package/repository-add "gnu"          "http://elpa.gnu.org/packages/")
  (serika-f/package/repository-add "melpa-stable" "https://stable.melpa.org/packages/")
  (serika-f/package/repository-add "melpa"        "http://melpa.org/packages/")

  (serika-f/package/initialize)
  (serika-f/package/list-update))

;; Init
(defun init ()
  (serika-c/eg/add :parents '("base require")
                   :name    'package-manager
                   :func    #'serika-g/package//require)

  (serika-c/eg/add :parents '("base configure")
                   :name    'package-manager
                   :func    #'serika-g/package//configure))
