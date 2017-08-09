;;; package --- list functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika-f/list/random (target-list)
  "Take random  atom from TARGET-LIST."
  (when (= (length target-list) 0)
    (error "Attempt to get random value from nil"))
  (car (nthcdr (random (length target-list)) target-list)))

(defun serika-f/list/until-t (list func)
  "Iterates through LIST, stops iteration when FUNC returned t.
Returns element where FUNC returned t, or nil if nobody was found."
  (cl-loop for elt in list
           until (funcall func elt)
           finally return elt))

(provide 'func-list)
;;; func-list.el ends here
