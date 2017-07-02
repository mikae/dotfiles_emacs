;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'virtualenvwrapper)

(defun init ()
  "Configure `virtualenvwrapper'."

  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "/home/data/tmp/.python-virtualenvs"))
