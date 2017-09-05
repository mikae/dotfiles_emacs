;;; package --- serika core configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'core-execution)
(require 'core-task)

(defun serika-c/init ()
  "Initialize `serika'."
  (serika-c/eg/create)

  (serika-c/task/add "zero")
  (serika-c/task/add "base")
  (serika-c/task/add "core")
  (serika-c/task/add "inf")
  (serika-c/task/execute-all)

  (serika-c/eg/execute)
  )

(provide 'core-serika)
;;; core-serika.el ends here
