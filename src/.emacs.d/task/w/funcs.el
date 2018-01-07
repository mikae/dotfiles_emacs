;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure last parts."
  (serika-c/eg/add-many-by-name 'load-local-config
    ("w")
    (progn
      (let ((--local-file-path (f-join user-emacs-directory
                                       "local-post.el")))
        (when (f-file-p --local-file-path)
          (load-file --local-file-path))))))
