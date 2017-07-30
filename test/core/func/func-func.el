;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-func)

(ert-deftest create-variable-toggler|simple ()
  (setq test-var nil)
  (let ((fun (serika-m/func/create-variable-toggler test-var)))
    (funcall fun)
    (should (eq test-var
                t))
    (funcall fun)
    (should (eq test-var
                nil)))
  )


(ert-deftest create-minor-mode-toggler|simple ()
  (define-minor-mode test-mode "")
  (let ((fun (serika-m/func/create-minor-mode-toggler test-mode)))
    (funcall fun)
    (should (eq test-mode
                t))
    (funcall fun)
    (should (eq test-mode
                nil))
  )
)
