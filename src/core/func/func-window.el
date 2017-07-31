;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/window/only-new-window ()
  "Delete all windows and create a new one."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*scratch*"))

(provide 'func-window)
;;; func-window.el ends here
