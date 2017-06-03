(defun serika-web-mode-top-language ()
  "Return the main language of `web-mode' buffer."
  (save-excursion
    (goto-char (point-min))
    (web-mode-language-at-pos)))

(defun serika-web-mode-buffer-is-html-p ()
  "Return t if `web-mode' buffer type is html."
  (string= (serika-web-mode-top-language) "html"))
