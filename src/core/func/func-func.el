;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Variables
(defvar --serika-lambda-hashtable (make-hash-table :test 'eq))

;; Functions
(defmacro serika-f/func/create-variable-toggler (var)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (setq ,var (not ,var))))


(defmacro serika-f/func/create-minor-mode-toggler (mmode)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (,mmode (if ,mmode
                 -1
               +1))))

(defun serika-f/func/create-ander (&rest funcs)
  "Return function that returns t if all FUNCS returned t."
  (lambda ()
    (cl-reduce (lambda (result func)
                 (and result
                      (funcall func)))
               funcs
               :initial-value t)))

(defmacro serika-f/func/bind (func &rest body)
  `(lambda ()
     (funcall ,func ,@body)))


(defmacro serika-f/func/lambda (name args &rest body)
  "If saved lambda associated with NAME exists, return it.
Otherwise, create it, associate it with NAME, save it, and return it.
If name is nil, just create and return new lambda."
  `(if ,name
       (let ((--lambda (gethash ,name --serika-lambda-hashtable)))
         (when (not --lambda)
           (setq --lambda (lambda ,args
                            (progn ,@body)))
           (puthash ,name --lambda --serika-lambda-hashtable))
         --lambda)
     (lambda ,args
       (progn ,@body))))

(defun serika-f/func/construct (&rest funcs)
  "Creates lambda, that executes all FUNCS."
  (dolist (obj funcs)
    (unless (functionp obj)
      (error "Funcs should be an array of functions.")))
  (lambda ()
    (dolist (func funcs)
      (funcall func))))

(defun serika-f/func/predicated (func predicate)
  "Create function, that wull execute FUNC only if
result of invocation of PREDICATE is t."
  (unless (and (functionp func)
               (functionp predicate))
    (error "FUNC and PREDICATE must be functions."))
  (lambda ()
    (when (funcall predicate)
      (funcall func))))

(provide 'func-func)
;;; func-func.el ends here
