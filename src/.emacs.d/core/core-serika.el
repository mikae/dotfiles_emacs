;;; package --- serika core configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'core-execution)
(require 'core-task)

(defun serika-c/init ()
  "Initialize `serika'."
  (serika-c/eg/create)

  (serika-c/task/add "-s(w)")
  (serika-c/task/add "-w")
  (serika-c/task/add "e")
  (serika-c/task/add "s(e)")
  (serika-c/task/add "s(s(e))")
  (serika-c/task/add "w")

  (serika-c/task/execute-all)

  (serika-c/eg/execute))

(provide 'core-serika)
;;; core-serika.el ends here
