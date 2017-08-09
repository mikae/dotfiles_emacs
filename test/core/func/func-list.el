;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-list)

;; `until-t'
(ert-deftest until-t|check-execution ()
  (let* ((--values '(1 2 3 4 5 6 7 8 9 0))
         (--list ())
         (--func (lambda (--item)
                   (setq --list (cons --item --list))
                   nil)))
    (serika-f/list/until-t --values --func)
    (should (equal --values
                   (reverse --list)))))

(ert-deftest until-t|check-stopped ()
  (let* ((--values '(1 2 3 4 5 6 7 8 9 0))
         (--list ())
         (--func (lambda (--item)
                   (setq --list (cons --item --list))
                   (= --item
                      5))))
    (serika-f/list/until-t --values --func)
    (should (equal (reverse --list)
                   '(1 2 3 4 5)))))

(provide 'func-list)
;;; func-list.el ends here
