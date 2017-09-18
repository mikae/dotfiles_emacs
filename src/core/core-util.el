;;; package --- Summary
;;; Commentary:
;;; Code:

(defun util/path-join (&rest parts)
  (let ((path (car parts)))
    (dolist (part (cdr parts))
      (setq path (concat (file-name-as-directory path)
                         part)))
    path))

(defun util/read-file-as-string (path)
  ""
  (unless (file-exists-p path)
    (error "Can't read file: file %s doesn't exist" path))

  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'core-util)
;;; core-funcs.el ends here
