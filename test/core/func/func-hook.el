;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-hook)
(ert-deftest add-oncely|simple ()
  (define-minor-mode test-mode-1 "")

  (let ((counter-1 0)
        (buffer-1 (generate-new-buffer "test-buffer-1"))
        (counter-2 0)
        (buffer-2 (generate-new-buffer "test-buffer-2")))
    (serika-f/hook/add-oncely 'test-mode-1-hook
                              (lambda ()
                                (setq counter-1 (1+ counter-1))))
    (with-current-buffer buffer-1
      (should (eq 0 counter-1))
      (test-mode-1 +1)
      (should (eq 1 counter-1))
      (test-mode-1 -1)
      (should (eq 1 counter-1))
      (test-mode-1 +1)
      (should (eq 1 counter-1)))

    (with-current-buffer buffer-2
      (should (eq 1 counter-1))
      (test-mode-1 +1)
      (should (eq 2 counter-1))
      (test-mode-1 -1)
      (should (eq 2 counter-1))
      (test-mode-1 +1)
      (should (eq 2 counter-1)))))
