;;; package --- buffer functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika-f/buffer/save-current ()
  "Save current buffer."
  (interactive)
  (save-buffer))

(defun serika-f/buffer/kill-current ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun serika-f/buffer/hide-current ()
  "Hide current buffer."
  (interactive)
  (previous-buffer))

(defun serika-f/buffer/save ()
  "Invoke buffer-save function for this buffer.
If nothing is here just save buffer."
  (interactive)
  (if (boundp 'serika-buffer-save-function)
      (funcall serika-buffer-save-function)
      (serika-f/buffer/save-current)))

(defun serika-f/buffer/kill ()
  "Invoke buffer-kill function for this buffer.
If nothing is here just kill buffer."
  (interactive)
  (if (boundp 'serika-buffer-kill-function)
      (funcall serika-buffer-kill-function)
      (serika-f/buffer/kill-current)))

(defun serika-f/buffer/hide ()
  "Invoke buffer-hide function for this buffer.
If nothing is here just hide buffer."
  (interactive)
  (if (boundp 'serika-buffer-hide-function)
      (funcall serika-buffer-hide-function)
      (serika-f/buffer/hide-current)))

(defun serika-f/buffer/kill-by-major-mode (mode)
  "Kill all buffer by major mode MODE."
  (mapc (lambda (buffer)
          (when (eq mode
                    (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun serika-f/buffer/exists-p (mode)
  "Return t if any buffer with MODE exists."
  (cl-reduce (lambda (a b)
               (or a b))
             (mapcar (lambda (buffer)
                       (eq mode
                           (buffer-local-value 'major-mode buffer)))
                     (buffer-list))))

(defun serika-f/buffer/not-exists-p (mode)
  "Return t if any buffer with MODE exists."
  (not (serika-f/buffer/exists-p mode)))

(provide 'func-buffer)
;;; func-buffer.el ends here
