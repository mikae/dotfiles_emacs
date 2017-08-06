;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/eldoc/activate ()
  "Activate `eldoc' in curreng buffer"
  (eldoc-mode +1))

(defmacro serika-f/eldoc/create-activator (&rest forms)
  "Create lambda, that activates eldoc-mode, and
executes FORMS after."
  `(lambda ()
     (eldoc-mode +1)
     (progn ,@forms)))

;; Global
(defun serika-gc/eldoc//require ()
  "Require modules for `eldoc'."
  (require 'eldoc))

(defun init ()
  "Configure `eldoc'."
  (serika-c/eg/add :parents '("require")
                   :name    'eldoc
                   :func    #'serika-gc/eldoc//require))
