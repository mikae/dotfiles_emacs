;;; package --- Summary
;;; Commentary:
;;; Code:

;; Private
(defun serika-f/eshell//update-counter ()
  "Update `serika-eshell--counter'."
  (setq serika-eshell--counter (1+ serika-eshell--counter)))

;; Public
(defun serika-f/eshell/execute (cmd &optional autokill)
  "Execute command CMD."
  (let* ((win (split-window-vertically (- (window-total-height) serika-eshell-window-height)))
         (bufname (format "eshell %d" serika-eshell--counter))
         (buf)
         (resultcmd cmd))
    (serika-f/eshell//update-counter)
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
