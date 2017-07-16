;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/package//require ()
  "Require modules."
  (require 'func-package)
  (message "require package")
  (message "require rel"))

(defun serika-g/package//configure ()
  "Configure package manager."
  (serika-g/package//require)
  (serika-f/package/repository-clear)

  (serika-f/package/repository-add "gnu"          "http://elpa.gnu.org/packages/")
  (serika-f/package/repository-add "melpa-stable" "https://stable.melpa.org/packages/")
  (serika-f/package/repository-add "melpa"        "http://melpa.org/packages/")

  (serika-f/package/initialize)
  (serika-f/package/list-update)

  (message "configure package")
  (message "configure rel"))

;; Init
(defun init ()
  (message "hoi")
  ;; (serika-c/eg/add :parents '("require")
  ;;                  :name    'require-package
  ;;                  :func    #'serika-g/package//require)

  (serika-c/eg/add :parents '("configure")
                   :name    'configure-package
                   :func    #'serika-g/package//configure)
  )
