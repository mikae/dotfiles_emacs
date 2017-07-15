;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/file/create (filepath &optional create-directory)
  "Create file with path FILEPATH"
  (let* ((expanded (expand-file-name filepath))
         (dir (directory-file-name (file-name-directory expanded))))
    (when (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    (when (not (file-exists-p dir))
      (if create-directory
          (make-directory dir t)
        (error "Cannot create file, directory %s doesn't exist" dir)))
    (write-region "" nil expanded t)))

(provide 'func-file)
;;; func-file.el ends here
