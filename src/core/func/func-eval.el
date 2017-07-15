;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/eval/file (file)
  "Execute FILE and return the result of the last expression."
  (load-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-max))
    (backward-sexp)
    (eval (sexp-at-point))))

(provide 'func-eval)
;;; func-eval.el ends here
