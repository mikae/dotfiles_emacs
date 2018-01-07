;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/window/delete-but-scratch ()
  "Delete all windows and go to scratch window."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  ;; TODO: kill other buffers as well
  )
