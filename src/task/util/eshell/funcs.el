;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/eshell//update-counter ()
  "Update `serika-eshell--counter'."
  (setq serika-eshell--counter (1+ serika-eshell--counter)))

(defun serika/eshell/execute (cmd &optional autokill)
  "Execute command CMD."
  (let* ((win (split-window-vertically (- (window-total-height) serika-eshell-window-height)))
         (bufname (format "eshell %d" serika-eshell--counter))
         (buf)
         (resultcmd cmd))
    (serika/eshell//update-counter)
    (with-selected-window win
      (when autokill
        (setq resultcmd (format "%s; (delete-window (get-buffer-window \"%s\")); exit" resultcmd bufname)))

      (setq buf (eshell))
      (switch-to-buffer buf)
      (rename-buffer bufname)

      (eshell-return-to-prompt)
      (insert resultcmd)
      (eshell-send-input)
      )))
