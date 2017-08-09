;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

(defun serika-f/package/make-sure-installed (package-name)
  "If PACKAGE-NAME is not installed, install it."
  (unless (package-installed-p package-name)
    (package-install package-name)))

(defun serika-f/package/configure ()
  "Clear repository list."
  (setq package-archives ())
  (setq package-pinned-packages ())
  (setq package-archive-priorities ()))

(defun serika-f/package/repository-add (name address &optional priority)
  "Add repository NAME with ADDRESS to list of repositories."
  (add-to-list 'package-archives `(,name . ,address))
  (when (and priority
             (numberp priority))
    (add-to-list 'package-archive-priorities `(,name . ,priority))))

(defun serika-f/package/initialize ()
  "Initialize package manager."
  (package-initialize))

(defun serika-f/package/list-update ()
  "Update list of packages."
  (unless package-archive-contents
    (package-refresh-contents)))

(defun serika-f/package/blacklist (&rest packages)
  "Prevent PACKAGE of being installed using package.el."
  (dolist (package packages)
    (add-to-list 'package-pinned-packages `(,package . ""))))

(provide 'core-package)
;;; func-package.el ends here
