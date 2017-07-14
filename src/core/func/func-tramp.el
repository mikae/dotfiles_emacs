;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/tramp/sudo-write (&optional filepath)
  (interactive)
  (let ((result-filepath (or filepath buffer-file-name)))
    (when result-filepath
      (write-file (concat "/sudo:root@localhost:" result-filepath)))))

(provide 'func-tramp)
;;; func-tramp.el ends here
