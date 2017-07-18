;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Path variables
(defvar serika-start-directory
  user-emacs-directory
  "Serika start directory.")

(defconst serika-core-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "core")))
  "Serika core directory.")

(defconst serika-tmp-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory ".tmp")))
  "Serika directory for temporary files.")

(defconst serika-task-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "task")))
  "Directory for serika tasks.")

(defconst serika-plugin-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "plugin")))
  "Directory for manual installed plugins.")

(defconst serika-conf-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "conf")))
  "Directory for various configuration files except `.el'.")

(defconst serika-assets-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "assets")))
  "Directory for asset files.")

(defconst serika-images-directory
  (expand-file-name (concat serika-start-directory
                            (file-name-as-directory "assets")
                            (file-name-as-directory "images")))
  "Directory for asset files.")

;; Mutating load-path
(let ((add-to-load-path (lambda (dir)
                          (add-to-list 'load-path dir))))
  (mapc add-to-load-path
        `(
          ,serika-core-directory
          ,serika-plugin-directory
          ,(concat serika-core-directory "func/")
          ))

  (let ((base serika-plugin-directory))
    (dolist (f (directory-files base))
      (let ((name (concat base f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (funcall add-to-load-path name)))))
  )

;;; core-load-path.el ends here
