;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Variables
(defvar --func-lambda-hashtable (make-hash-table :test 'eq))

;; Functions
(defmacro func/func/create-variable-toggler (var)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (setq ,var (not ,var))))

(defmacro func/func/toggle-minor-mode (mmode)
  "Toggle VAR."
  `(,mmode (if ,mmode
               -1
             +1)))

(defmacro func/func/lambda (name args &rest body)
  "If saved lambda associated with NAME exists, return it.
Otherwise, create it, associate it with NAME, save it, and return it.
If name is nil, just create and return new lambda."
  `(if ,name
       (let ((--lambda (gethash ,name --func-lambda-hashtable)))
         (when (not --lambda)
           (setq --lambda (lambda ,args
                            (progn ,@body)))
           (puthash ,name --lambda --func-lambda-hashtable))
         --lambda)
     (lambda ,args
       (progn ,@body))))



(defmacro func/func/require (&rest modules)
  "Create lambda that requires modules."
  `(cl-loop for --module in ',modules
            do
            (require (nth 1 --module)))) ;; --module === (quote module-name)
