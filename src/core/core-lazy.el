;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-execution)
(require 'core-execution)

(defvar --serika-filetype-pattern-alist (list))

(defun serika-c/lazy/filetype-hook ()
	"Filetype hook."
	(let ((buffer-name (file-name-nondirectory (buffer-file-name))))
		(dolist (item --serika-filetype-pattern-alist)
			(when (string-match (car item) buffer-name)
				(funcall (cdr item))
				(assq-delete-all (car item) --serika-filetype-pattern-alist)))))

(defun serika-c/lazy/configure ()
	"Configure laziness."
	(dolist (item `(("\\.py\\'" . ,(eg/event-node --serika-execution-graph
																								:name 'ft-python))))
		(add-to-list '--serika-filetype-pattern-alist item))
	(add-hook 'find-file-hook #'serika-c/lazy/filetype-hook))

(provide 'core-lazy)
;;; core-lazy.el ends here
