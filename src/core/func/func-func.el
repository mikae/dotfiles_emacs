;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
     (funcall ,func ,@body)
     ))

(provide 'func-func)
;;; func-func.el ends here
