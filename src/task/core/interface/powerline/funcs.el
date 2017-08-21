;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/powerline//require ()
  "Require modules for `powerline'."
  (require 'powerline))

(defun serika-g/powerline//configure ()
  "Require modules for `powerline'."
  nil)

(defun init ()
  "Configure `powerline'."
  (serika-c/eg/add-install :package-list '(powerline)
                           :name         'powerline
                           :parents      '("base interface install"))

  (serika-c/eg/add-many-by-parents ("base interface install powerline")
                                   'require
                                   #'serika-g/powerline//require

                                   'configure
                                   #'serika-g/powerline//configure))
