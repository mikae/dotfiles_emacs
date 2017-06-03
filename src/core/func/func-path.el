;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun serika/path/join (&rest joined-path-parts)
  "Return path join of all JOINED-PATH-PARTS."
  (let ((test-fun (lambda (first second)
                    (concat (file-name-as-directory first) second))))
    (cl-reduce test-fun joined-path-parts)))


(provide 'func-path)
;;; func-path.el ends here
