;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/string/resolve-url (str)
  (if (stringp str)
      (replace-regexp-in-string "%2b" "+" str)
    (error "STR should be a string")))

(provide 'func-string)
;;; func-string.el ends here
