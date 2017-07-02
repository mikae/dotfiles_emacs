;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'func-system)

(defun serika/path/join (&rest joined-path-parts)
  "Return path join of all JOINED-PATH-PARTS."
  (let ((test-fun (lambda (first second)
                    (concat (file-name-as-directory first) second))))
    (cl-reduce test-fun joined-path-parts)))

(defun serika/path/home-join (&rest joined-path-parts)
  (serika/path/join (cons (serika/system/user-home) joined-path-parts)))


(provide 'func-path)
;;; func-path.el ends here
