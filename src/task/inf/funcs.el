;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure last parts."
  (serika-c/eg/add-many-by-name 'load-local-config
                                ("inf")
                                (lambda ()
                                  "Load local configuration."
                                  (let ((--local-file-path (concat user-emacs-directory
                                                                   "local-post.el")))
                                    (when (file-exists-p --local-file-path)
                                      (load-file --local-file-path))))))
