;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika/image/scroll-up (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-up 1))

(defun serika/image/scroll-right (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-right 1))

(defun serika/image/scroll-bottom (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-bottom 1))

(defun serika/image/scroll-left (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-left 1))

;; Global
(defun serika/image//auto-mode-alist ()
  "Configure `auto-mode-alist' for image files.")

(defun serika/image//keymap ()
  "Configure `image-mode-map'."
  (setq image-mode-map (let ((map (make-sparse-keymap)))
                         (define-key image-mode-map (kbd "q")     #'serika/buffer/kill-current)

                         (define-key image-mode-map (kbd "h")     #'serika/image/scroll-left)
                         (define-key image-mode-map (kbd "j")     #'serika/image/scroll-down)
                         (define-key image-mode-map (kbd "k")     #'serika/image/scroll-up)
                         (define-key image-mode-map (kbd "l")     #'serika/image/scroll-right)

                         (define-key image-mode-map (kbd "A-J")   #'image-next-file)
                         (define-key image-mode-map (kbd "A-K")   #'image-previous-file)

                         (define-key image-mode-map (kbd "C-c p") (lambda ()
                                                                    (interactive)
                                                                    (run-associated-program buffer-file-name))))))

(defun init ()
  "Configure `image-mode'."
  (serika/image//auto-mode-alist)
  (serika/image//keymap))
