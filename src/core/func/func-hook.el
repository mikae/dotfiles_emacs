;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika-f/add-hook-oncely (hook func &optional to-beginning)
  (let* ((--hook   hook)
         (--evaluated nil)
         (--func   func)
         (--func-1 (lambda ()
                     (unless --evaluated
                       (funcall --func)
                       (setq --evaluated t))))
         (--func-2 (lambda ()
                     (when --evaluated
                       (remove-hook --hook --func-1)))))
    (add-hook --hook --func-1 (and (not to-beginning) t))
    (add-hook --hook --func-2 (and (not to-beginning) t))))

(defun serika-f/add-hook-predicated (hook func predicate &optional to-beginning)
  (let* ((--hook hook)
         (--fun  func)
         (--pred predicate)
         (--func (lambda ()
                   (when (funcall --pred)
                     (funcall --fun)))))
    (add-hook --hook --func (and (not to-beginning) t))))

(defun serika-f/add-hook (hook func &optional to-beginning)
  (add-hook hook func (and (not to-beginning) t)))


(provide 'func-hook)
;;; func-hook.el ends here
