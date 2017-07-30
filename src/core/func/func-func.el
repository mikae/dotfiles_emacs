;;; package --- Summary
;;; Commentary:
;;; Code:

(defmacro serika-m/func/create-variable-toggler (var)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (setq ,var (not ,var))))


(defmacro serika-m/func/create-minor-mode-toggler (mmode)
  "Toggle VAR."
  `(lambda ()
     (interactive)
     (,mmode (if ,mmode
                 -1
               +1))))

(provide 'func-func)
;;; func-func.el ends here
