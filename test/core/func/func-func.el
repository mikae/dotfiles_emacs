;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-func)

(ert-deftest create-variable-toggler|simple ()
  (setq test-var nil)
  (let ((fun (serika-f/func/create-variable-toggler test-var)))
    (funcall fun)
    (should (eq test-var
                t))
    (funcall fun)
    (should (eq test-var
                nil)))
  )


(ert-deftest create-minor-mode-toggler|simple ()
  (define-minor-mode test-mode "")
  (let ((fun (serika-f/func/create-minor-mode-toggler test-mode)))
    (funcall fun)
    (should (eq test-mode
                t))
    (funcall fun)
    (should (eq test-mode
                nil))
  ))

(ert-deftest create-ander|check-function-invocation ()
  (let* ((test-1)
         (test-2)
         (fun-1 (lambda () (setq test-1 t)))
         (fun-2 (lambda () (setq test-2 t)))
         (ander (serika-f/func/create-ander fun-1
                                            fun-2)))
    (funcall ander)
    (should (eq t
                test-1))
    (should (eq t
                test-2))))

(ert-deftest create-ander|truth-table-test ()
  (let* ((--1   (lambda () t))
         (--0   (lambda () nil))
         (--00  (serika-f/func/create-ander --0
                                            --0))
         (--01  (serika-f/func/create-ander --0
                                            --1))
         (--10  (serika-f/func/create-ander --1
                                            --0))
         (--11  (serika-f/func/create-ander --1
                                            --1)))
    (should (eq nil
                (funcall --00)))
    (should (eq nil
                (funcall --01)))
    (should (eq nil
                (funcall --10)))
    (should (eq t
                (funcall --11)))))
