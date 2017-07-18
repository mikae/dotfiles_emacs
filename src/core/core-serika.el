;;; package --- serika core configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(require 'core-execution)
(require 'core-task)
(require 'core-splash)

(defun serika-c/init ()
  "Initialize `serika'."
  (serika-c/eg/create)

  (serika-c/task/add "util")
  (serika-c/task/add "base")
  (serika-c/task/add "core")
  (serika-c/task/execute-all)

  (serika-c/eg/execute)
  (serika-c/splash/configure)
)

(provide 'core-serika)
;;; core-serika.el ends here
