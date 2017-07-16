;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/evil-nerd-commenter//require ()
  "Require modules for `evil-nerd-commenter'."
  (require 'evil-nerd-commenter))

;; Init
(defun init ()
  "Configure `evil-nerd-commenter'."
  (serika-g/evil-nerd-commenter//require))
