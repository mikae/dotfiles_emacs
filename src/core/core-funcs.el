;;; package --- Summary
;;; Commentary:
;;; Code:

(defun func/path-join (&rest parts)
  (let ((path (car parts)))
    (dolist (part (cdr parts))
      (setq path (concat (file-name-as-directory path)
                         part)))
    path))

(provide 'core-funcs)
;;; core-funcs.el ends here
