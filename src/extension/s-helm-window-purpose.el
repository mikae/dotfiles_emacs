;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'helm-buffers)

(defconst s-purpose-version 0.1
  "Version of helm-window-purpose.")

(defvar s-purpose-layout-dirs nil
  "Directories where window-purpose should be located in.")

(defvar s-layout-helm-source `((name       . "helm-window-purpose-layout")
                               (candidates . s//purpose-layout-get-candidates)
                               (action     . (("Change layout" . s//purpose-layout-change))))
  "Return available layout candidates.")

(defvar s-layout-fallback-source `((name   . "helm-window-purpose-layout fallback source")
                                   (dummy)
                                   (action . (("Nothing" . s//purpose-layout-default-action))))
  "Fallback source.")

(defun s//purpose-layout-change (layout-data)
  "Load layout associated with LAYOUT-DATA."
  (when (and (listp layout-data)
             (stringp (car layout-data)))
    (purpose-x-code1-setup)
    (purpose-load-window-layout-file (car layout-data))))

(defun s//purpose-layout-default-action ()
  "Default action for changing window purpose layout."
  (message "No window-purpose layout was selected to change."))

(defun s//purpose-layout-get-candidates ()
  "Get candidates for changing layout."
  (unless (listp s-purpose-layout-dirs)
    (error "beda. vse ploho. `s-purpose-layout-dirs' is not a list :(("))
  (let ((files))
    (dolist (dir s-purpose-layout-dirs)
      (setq files (nconc files
                         (mapcar (lambda (elem)
                                   `(,(let ((name (file-name-nondirectory elem)))
                                        ;; todo: rewrite
                                        (substring name 0 (- (length name)
                                                             15)))
                                     ,elem))
                                 (cl-remove-if-not (lambda (elem)
                                                     (string-match "\\.purpose-layout$" elem))
                                                   (directory-files dir t))))))
    files))

(defun s/purpose-layout-helm ()
  "Show `helm' buffer to change layout."
  (interactive)
  (helm :sources '(s-layout-helm-source)))

(provide 's-helm-window-purpose)
;;; s-helm-window-purpose.el ends here
