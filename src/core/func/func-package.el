;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

(defun serika-f/package/make-sure-installed (package-name)
  "If PACKAGE-NAME is not installed, install it."
  (unless (package-installed-p package-name)
    (package-install package-name)))

(defun serika-f/package/repository-clear ()
  "Clear repository list."
  (setq package-archives ()))

(defun serika-f/package/repository-add (name address)
  "Add repository NAME with ADDRESS to list of repositories."
  (add-to-list 'package-archives `(,name . ,address)))

(defun serika-f/package/initialize ()
  "Initialize package manager."
  (package-initialize))

(defun serika-f/package/list-update ()
  "Update list of packages."
  (unless package-archive-contents
    (package-refresh-contents)))

(provide 'func-package)
;;; func-package.el ends here
