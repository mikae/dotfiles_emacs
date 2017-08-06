;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/file/create (filepath &optional create-parent-directories)
  "Create file with path FILEPATH"
  (let* ((expanded (expand-file-name filepath))
         (dir (directory-file-name (file-name-directory expanded))))
    (when (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    (when (not (file-exists-p dir))
      (if create-parent-directories
          (make-directory dir t)
        (error "Cannot create file, directory %s doesn't exist" dir)))
    (write-region "" nil expanded t)))

(defun serika-f/file/readable-p (filepath)
  "Return t, if FILEPATH is readable."
  (and (stringp filepath)
       (file-exists-p filepath)))

(defun serika-f/file/read-as-string (filepath)
  "Return filePath's file content."
  (unless (serika-f/file/readable-p filepath)
    (error "Can't read file: %s" filepath))

  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(provide 'func-file)
;;; func-file.el ends here
