;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/virtualenvwrapper//require ()
  "Require modules for `virtualenvwrapper'."
  (require 'virtualenvwrapper))

(defun serika-g/virtualenvwrapper//configure ()
  "Configure `virtualenwrapper'."
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "/home/data/tmp/.python-virtualenvs"))

;; Init
(defun init ()
  "Configure `virtualenvwrapper'."
  (serika-g/virtualenvwrapper//require)
  (serika-g/virtualenvwrapper//configure))
