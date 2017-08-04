;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-string)

(ert-deftest resolve-url|check-error ()
  (should-error (serika-f/string/resolve-url 't)))

(ert-deftest resolve-url|check ()
  (let ((--values '(("test%2b.el" . "test+.el")
                    ("test%2b.el" . "test+.el"))))
    (dolist (elem --values)
      (should (string= (serika-f/string/resolve-url (car elem))
                       (cdr elem))))))
