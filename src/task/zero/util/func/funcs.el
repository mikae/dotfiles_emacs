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


(defmacro func/func/create-minor-mode-toggler (mmode)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (,mmode (if ,mmode
                 -1
               +1))))

(defun func/func/create-ander (&rest funcs)
  "Return function that returns t if all FUNCS returned t."
  (lambda ()
    (cl-reduce (lambda (result func)
                 (and result
                      (funcall func)))
               funcs
               :initial-value t)))

(defmacro func/func/bind (func &rest body)
  `(lambda ()
     (funcall ,func ,@body)))


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

(defun func/func/construct (&rest funcs)
  "Creates lambda, that executes all FUNCS."
  (dolist (obj funcs)
    (unless (functionp obj)
      (error "Funcs should be an array of functions.")))
  (lambda ()
    (dolist (func funcs)
      (funcall func))))

(defun func/func/predicated (func predicate)
  "Create function, that wull execute FUNC only if
result of invocation of PREDICATE is t."
  (unless (and (functionp func)
               (functionp predicate))
    (error "FUNC and PREDICATE must be functions."))
  (lambda ()
    (when (funcall predicate)
      (funcall func))))

(defmacro func/func/requirer (&rest modules)
  "Create lambda that requires modules."
  `(cl-loop for --module in ',modules
            do
            (require (nth 1 --module)))) ;; --module === (quote module-name)
