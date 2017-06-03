;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika/symbol/concat (first second)
  "Concat FIRST and SECOND as symbols."
  (make-symbol (concat (or (and (symbolp first)  (symbol-name first))  "")
                       (or (and (symbolp second) (symbol-name second)) ""))))

(defun serika/symbol/name (symbol)
  "Get symbol name of SYMBOL."
  (or (and (symbolp symbol) (symbol-name symbol)) ""))

(provide 'func-symbol)
;;; func-symbol.el ends here
