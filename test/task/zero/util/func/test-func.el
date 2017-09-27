;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'buttercup)

(describe "func/func/toggle-minor-mode"
  (it "Toggles minor mode"
    (define-minor-mode test-mode-1 "")
    (func/func/toggle-minor-mode test-mode-1)
    (expect test-mode-1
            :to-be t)
    (func/func/toggle-minor-mode test-mode-1)
    (expect test-mode-1
            :to-be nil)))

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
