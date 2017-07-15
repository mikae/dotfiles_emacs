;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'func-system)

(defun serika-f/path/join (&rest joined-path-parts)
  "Return path joining of all JOINED-PATH-PARTS."
  (let ((test-fun (lambda (first second)
                    (concat (file-name-as-directory first) second))))
    (cl-reduce test-fun joined-path-parts)))

(defun serika-f/path/home-join (&rest joined-path-parts)
  "Return path joining of JOINED-PATH-PARTS from user directory."
  ;; TODO: make it better
  (serika-f/path/join (serika-f/system/user-home) (apply 'serika-f/path/join joined-path-parts)))


(provide 'func-path)
;;; func-path.el ends here
