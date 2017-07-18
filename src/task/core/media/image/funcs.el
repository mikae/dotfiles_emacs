;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/image/scroll-up (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-up 1))

(defun serika-f/image/scroll-right (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-right 1))

(defun serika-f/image/scroll-bottom (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-bottom 1))

(defun serika-f/image/scroll-left (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-left 1))

;; Global
(defun serika-g/image//settings ()
  "Configure `auto-mode-alist' for image files."
  nil)

(defun serika-g/image//auto-mode-alist ()
  "Configure `auto-mode-alist' for image files."
  (add-to-list 'auto-mode-alist '("\\.bmp\\'"  . image-mode))
  (add-to-list 'auto-mode-alist '("\\.jpg\\'"  . image-mode))
  (add-to-list 'auto-mode-alist '("\\.jpeg\\'" . image-mode))
  (add-to-list 'auto-mode-alist '("\\.png\\'"  . image-mode)))

(defun serika-g/image//keymap ()
  "Configure `image-mode-map'."
  (setq image-mode-map (let ((map (make-sparse-keymap)))
                         ;; Quit from image viewing
                         (define-key map (kbd "q")     #'serika-f/buffer/kill-current)

                         (define-key map (kbd "h")     #'serika-f/image/scroll-left)
                         (define-key map (kbd "j")     #'serika-f/image/scroll-down)
                         (define-key map (kbd "k")     #'serika-f/image/scroll-up)
                         (define-key map (kbd "l")     #'serika-f/image/scroll-right)

                         (define-key map (kbd "A-J")   #'image-next-file)
                         (define-key map (kbd "A-K")   #'image-previous-file)

                         (define-key map (kbd "C-c p") (lambda ()
                                                         (interactive)
                                                         (run-associated-program buffer-file-name)))
                         map)))

(defun init ()
  "Configure `image-mode'."
  (serika-c/eg/add :parents '("settings")
                   :name    'image
                   :func    #'serika-g/image//settings)

  (serika-c/eg/add :parents '("settings image")
                   :name    'auto-mode-alist
                   :func    #'serika-g/image//auto-mode-alist)

  (serika-c/eg/add :parents '("keymap")
                   :name    'image
                   :func    #'serika-g/image//keymap))
