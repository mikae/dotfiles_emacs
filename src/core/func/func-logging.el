;;; package --- Summary
;;; Commentary:
;;; Code:

;; Variables
(defvar serika-logging-format "%m"
  "Format of log message.
%m - log message.")

;; Functions
(defun serika/logging/log (formatter &rest args)
  "Log message."
  (message ()))

(provide 'func-logging)
;;; func-logging.el ends here
