;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Pre configuration."
  (serika-c/eg/add-many-by-name 'load-local-pre-config
                                ("-inf")
                                (lambda ()
                                  "Load local configuration."
                                  (let ((--local-file-path (concat user-emacs-directory
                                                                   "local-pre.el")))
                                    (when (file-exists-p --local-file-path)
                                      (load-file --local-file-path))))))
