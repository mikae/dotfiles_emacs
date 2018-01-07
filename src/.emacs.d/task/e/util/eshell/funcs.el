;;; package --- Summary
;;; Commentary:
;;; Code:

;; Vars
(defvar serika-eshell-window-height 10
  "Height of vertically splitted eshell window.")

(defvar serika-eshell--counter 0
  "Counter of eshell sessions.")

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
