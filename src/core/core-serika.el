;;; package --- serika core configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(require 'core-task)
(require 'core-splash)

(defun serika/init ()
  "Initialize `serika'."
  (serika/task/add 'base)
  (serika/task/add 'core)
  (serika/task/execute-all)
  (serika/splash/configure)
)

(provide 'core-serika)
;;; core-serika.el ends here
