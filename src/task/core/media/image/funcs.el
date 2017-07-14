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
  "Configure `auto-mode-alist' for image files."
  (add-to-list 'auto-mode-alist '("\\.bmp\\'"  . image-mode))
  (add-to-list 'auto-mode-alist '("\\.jpg\\'"  . image-mode))
  (add-to-list 'auto-mode-alist '("\\.jpeg\\'" . image-mode))
  (add-to-list 'auto-mode-alist '("\\.png\\'"  . image-mode)))

(defun serika/image//keymap ()
  "Configure `image-mode-map'."
  (setq image-mode-map (let ((map (make-sparse-keymap)))
                         ;; Quit from image viewing
                         (define-key map (kbd "q")     #'serika/buffer/kill-current)

                         (define-key map (kbd "h")     #'serika/image/scroll-left)
                         (define-key map (kbd "j")     #'serika/image/scroll-down)
                         (define-key map (kbd "k")     #'serika/image/scroll-up)
                         (define-key map (kbd "l")     #'serika/image/scroll-right)

                         (define-key map (kbd "A-J")   #'image-next-file)
                         (define-key map (kbd "A-K")   #'image-previous-file)

                         (define-key map (kbd "C-c p") (lambda ()
                                                         (interactive)
                                                         (run-associated-program buffer-file-name)))
                         map)))

(defun init ()
  "Configure `image-mode'."
  (serika/image//auto-mode-alist)
  (serika/image//keymap))
