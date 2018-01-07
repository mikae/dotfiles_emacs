;;; package --- buffer functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun func/buffer/kill ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun func/buffer/kill-by-major-mode (mode)
  "Kill all buffer by major mode MODE."
  (mapc (lambda (buffer)
          (when (eq mode
                    (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun func/buffer/exists-p (mode)
  "Return t if any buffer with MODE exists."
  (cl-reduce (lambda (a b)
               (or a b))
             (mapcar (lambda (buffer)
                       (eq mode
                           (buffer-local-value 'major-mode buffer)))
                     (buffer-list))))

(defun func/buffer/not-exists-p (mode)
  "Return t if any buffer with MODE exists."
  (not (func/buffer/exists-p mode)))

(defun func/buffer/focus-to (mode)
  "Focus window with MAJOR-MODE."
  (let ((item (func/list/until-t (window-list)
                                     (func/func/lambda 'func/buffer/focus-to
                                                           (--window)
                                                           (eq mode
                                                               (buffer-local-value 'major-mode
                                                                                   (window-buffer --window)))))))
    (when item
      (select-window item))))

(defun func/buffer/check-modes (&rest mode-list)
  "Return t if current buffer is one of modes in MODE-LIST."
  (cl-reduce (func/func/lambda 'func/buffer/check-modes
                                   (val mode)
                                   (or val
                                       (eq major-mode
                                           mode)))
             mode-list
             :initial-value nil))

(defun func/buffer/last-string (&optional count)
  "Return last string."
  (save-excursion
    (goto-char (point-max))

    (dotimes (counter (or (1- (or count 0)) 0))
      (search-backward "\n" nil t))

    (buffer-substring (if (search-backward "\n" nil t)
                          (1+ (point))
                        1)
                      (point-max))))

(defun func/buffer/save-function (&optional func)
  "Set/get buffer save function."
  (if (functionp func)
      (set (make-local-variable '--func-buffer-save-function) func)
    --func-buffer-save-function))

(defun func/buffer/invoke-save-function ()
  "Invokes buffer save function."
  (interactive)
  (if (and (boundp '--func-buffer-save-function)
           (functionp --func-buffer-save-function))
      (funcall --func-buffer-save-function)
    (message "No internal save functions")))
