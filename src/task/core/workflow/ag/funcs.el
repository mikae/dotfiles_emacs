;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika-gc/ag//require ()
  "Require modules for `ag'."
  (require 'ag))

(defun serika-gc/ag//settings ()
  "Configure settings for `ag'."
  (setq ag-highlight-search t)
  (setq ag-reuse-window t)
  (setq ag-reuse-buffers t)

  (setq ag-arguments ag-arguments)
  (setq ag-executable (serika-f/system/getenv "PATH_EXECUTABLE_AG")))

(defun serika-gc/ag//global-keymap ()
  "Configure global keyamps for `ag'."
  (global-set-key (kbd "C-f a f")     #'ag-files)
  (global-set-key (kbd "C-f a r")     #'ag-regexp)
  (global-set-key (kbd "C-f a a")     #'ag)

  (global-set-key (kbd "C-f a d d")   #'ag-dired)
  (global-set-key (kbd "C-f a d r")   #'ag-dired-regexp)

  (global-set-key (kbd "C-f a p p")   #'ag-project)
  (global-set-key (kbd "C-f a p f")   #'ag-project-files)
  (global-set-key (kbd "C-f a p r")   #'ag-project-regexp)
  (global-set-key (kbd "C-f a p a")   #'ag-project-at-point)

  (global-set-key (kbd "C-f a p d d") #'ag-project-dired)
  (global-set-key (kbd "C-f a p d r") #'ag-project-dired-regexp)
  (global-set-key (kbd "C-f a p d d") #'ag-project-dired))

(defun init ()
  "Configure `ag'."
  (serika-c/eg/add-install :package-list '(ag)
                           :name         'ag)

  (serika-c/eg/add :parents '("require")
                   :name    'ag
                   :func    #'serika-gc/ag//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'ag
                   :func    #'serika-gc/ag//settings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'ag
                   :func    #'serika-gc/ag//global-keymap)
  )
