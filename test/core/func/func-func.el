;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-func)

;; `create-variable-toggler'
(ert-deftest create-variable-toggler|simple ()
  (setq test-var nil)
  (let ((fun (serika-f/func/create-variable-toggler test-var)))
    (funcall fun)
    (should (eq test-var
                t))
    (funcall fun)
    (should (eq test-var
                nil))))

;; `create-minor-mode-toggler'
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

;; `create-ander'
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

;; `bind'
(ert-deftest bind|bind-lambda ()
  (let* ((fun-1 (lambda (a b)
                  (+ a b)))
         (fun-2 (serika-f/func/bind fun-1
                                    1 2)))
    (should (eq (funcall fun-2)
                3))))

(ert-deftest bind|bind-func ()
  (let ((fun (serika-f/func/bind '+
                                 1 2 3)))
    (should (eq (funcall fun)
            6))))

;; `lambda'
(ert-deftest lambda|creates-lambda ()
  (let* ((value)
         (fun (serika-f/func/lambda nil
                                    ()
                                    (setq value t))))
    (should fun)
    (funcall fun)
    (should (eq value
                t))
    ))

(ert-deftest lambda|saves-lambda ()
  (let ((fun-1 (serika-f/func/lambda 'nya
                                     ()
                                     ()))
        (fun-2 (serika-f/func/lambda 'nya
                                     ()
                                     ())))
    (should fun-1)
    (should fun-2)
    (should (eq fun-1
                fun-2))))

;; `construct'
(ert-deftest construct|check-errors ()
  (should-error (serika-f/func/construct 1)))

(ert-deftest construct|check-constructed ()
  (let* ((value       "")
         (fun-1       (lambda ()
                        (setq value (concat value "1"))))
         (fun-2       (lambda ()
                        (setq value (concat value "2"))))
         (fun-3       (lambda ()
                        (setq value (concat value "3"))))
         (constructed (serika-f/func/construct fun-1
                                               fun-2
                                               fun-3)))
    (funcall constructed)
    (should (string= value
                     "123"))))

;; `predicated'
(ert-deftest predicated|check-errors ()
  (should-error (serika-f/func/predicated 1      1))
  (should-error (serika-f/func/predicated (lambda ()) 1))
  (should-error (serika-f/func/predicated 1      (lambda ()))))

(ert-deftest predicated|check ()
  (let* ((counter  0)
         (pred-t   (lambda () t))
         (pred-nil (lambda () nil))
         (func     (lambda () (setq counter (1+ counter)))))
    (funcall (serika-f/func/predicated func pred-t))
    (should (eq counter
                1))
    (funcall (serika-f/func/predicated func pred-nil))
    (should (eq counter
                1))))
