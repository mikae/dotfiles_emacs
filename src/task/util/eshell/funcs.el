;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/eshell/execute (cmd &optional height)
  "Execute command CMD. HEIGHT is optional height of the window."
  (let* ((win (split-window-vertically (- (window-total-height) (or height 10))))
         (bufname  (concat "buffer \"" cmd "\""))
         (buf))
    (with-selected-window win
      (setq buf (eshell))
      (switch-to-buffer buf)
      (rename-buffer bufname)

      (eshell-return-to-prompt)
      (insert cmd)
      (eshell-send-input))))
