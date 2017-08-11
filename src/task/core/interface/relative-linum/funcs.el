;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/linum-relative/activate ()
  "Activate relative linum."
  (linum-on)
  (linum-relative-on))

;; Init
(defun init ()
  "Configure `linum'."
  (serika-c/eg/add-install :type         'package
                           :package-list '(linum-relative)
                           :name         'linum-relative)

  (serika-c/eg/add-many 'linum-relative
                        ("require")
                        (lambda ()
                          (require 'linum-relative))

                        ("settings")
                        (lambda ()
                          (setq linum-relative-current-symbol ""))))
