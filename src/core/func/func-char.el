;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/char/forward ()
  "Forward char if next char is not a \n char."
  (interactive)
  (when (not (eolp))
    (forward-char)))

(defun serika-f/char/backward ()
  "Forward char if next char is not a \n char."
  (interactive)
  (when (not (bolp))
    (backward-char)))

(provide 'func-char)
;;; func-char.el ends here
