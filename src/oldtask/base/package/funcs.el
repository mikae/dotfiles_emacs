;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/package//require ()
  "Require modules."
  (require 'func-package))

;; Init
(defun init ()
  "Configure package manager."
  (serika-g/package//require)
  (serika-f/package/repository-clear)

  (serika-f/package/repository-add "gnu"          "http://elpa.gnu.org/packages/")
  (serika-f/package/repository-add "melpa-stable" "https://stable.melpa.org/packages/")
  (serika-f/package/repository-add "melpa"        "http://melpa.org/packages/")

  (serika-f/package/initialize)
  (serika-f/package/list-update))
