;;; package --- Summary
;;; Commentary:
;;; Code:

(defmacro combine-and (&rest funcs)
  `(lambda ()
     (let ((--result t)
           (--index  0)
           (--length (length funcs)))
       (while (and --result
                   (< --index --length))
         (setq --result (and --result
                             (funcall (nth --index funcs))))
         (setq --index (1+ --index))))))

(provide 'func-func)
;;; func-func.el ends here
