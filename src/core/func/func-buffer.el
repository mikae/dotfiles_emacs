;;; package --- buffer functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika/buffer/save-current ()
  "Save current buffer."
  (interactive)
  (save-buffer))

(defun serika/buffer/kill-current ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun serika/buffer/hide-current ()
  "Hide current buffer."
  (interactive)
  (previous-buffer))

(defun serika/buffer/save ()
  "Invoke buffer-save function for this buffer.
If nothing is here just save buffer."
  (interactive)
  (if (boundp 'serika-buffer-save-function)
      (funcall serika-buffer-save-function)
      (serika/buffer/save-current)))

(defun serika/buffer/kill ()
  "Invoke buffer-kill function for this buffer.
If nothing is here just kill buffer."
  (interactive)
  (if (boundp 'serika-buffer-kill-function)
      (funcall serika-buffer-kill-function)
      (serika/buffer/kill-current)))

(defun serika/buffer/hide ()
  "Invoke buffer-hide function for this buffer.
If nothing is here just hide buffer."
  (interactive)
  (if (boundp 'serika-buffer-hide-function)
      (funcall serika-buffer-hide-function)
      (serika/buffer/hide-current)))

(defun serika/buffer/kill-by-major-mode (mode)
  "Kill all buffer by major mode MODE."
  (mapc (lambda (buffer)
          (when (eq mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(provide 'func-buffer)
;;; func-buffer.el ends here
