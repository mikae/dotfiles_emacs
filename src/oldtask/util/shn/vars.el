;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar serika-shn--file-format "%n-%p-%t"
  "Format of result shn files.")

(defvar serika-shn--executable (cond ((executable-find "shntool"))
                                     (t nil))
  "Executable to work with various `shntool files'.")
