;;; package --- Summary
;;; Commentary:
;;; Code:

(defun func/path-join (&rest parts)
  (let ((path (car parts)))
    (dolist (part (cdr parts))
      (setq path (concat (file-name-as-directory path)
                         part)))
    path))

(defun func/string-parse-url (url-string)
  (replace-regexp-in-string "%2b" "+" (file-name-nondirectory url-string)))

(provide 'core-funcs)
;;; core-funcs.el ends here
