;;; package --- list functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun serika/list/random (target-list)
  "Take random  atom from TARGET-LIST."
  (when (= (length target-list) 0)
    (error "Attempt to get random value from nil"))
  (car (nthcdr (random (length target-list)) target-list)))

(provide 'func-list)
;;; func-list.el ends here
