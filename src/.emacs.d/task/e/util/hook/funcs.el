;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun func/hook/add-oncely (hook func &optional to-beginning)
  "Add hooks, that will execute oncely in buffer."
  (let* ((--orig func)
         (--func (lambda ()
                   (when (or (not (local-variable-p        '--hooks-executed))
                             (not (local-variable-if-set-p '--hooks-executed))
                             (not (buffer-local-value      '--hooks-executed
                                                           (current-buffer))))
                     (set (make-local-variable '--hooks-executed) t)
                     (funcall --orig)))))
    (add-hook hook --func)))

(defun func/hook/add-predicated (hook func predicate &optional to-beginning)
  "Add hooks, that will execute if predicate is satisfied."
  (let* ((--hook hook)
         (--orig func)
         (--pred predicate)
         (--to-end (not to-beginning))
         (--func (lambda ()
                   (when (funcall --pred)
                     (funcall --orig)))))
    (add-hook --hook --func --to-end)))

(defun func/hook/add (hook func &optional to-beginning)
  "Add hook."
  (add-hook hook func (and (not to-beginning) t)))
