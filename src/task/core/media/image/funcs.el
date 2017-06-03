;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika/image/keymap ()
  "Configure `image-mode-map'."
  (setq image-mode-map (make-sparse-keymap))

  (define-key image-mode-map (kbd "q")   'serika/buffer/kill-current)

  (define-key image-mode-map (kbd "h")   (lambda () (interactive) (scroll-right 1)))
  (define-key image-mode-map (kbd "j")   (lambda () (interactive) (scroll-up 1)))
  (define-key image-mode-map (kbd "k")   (lambda () (interactive) (scroll-down 1)))
  (define-key image-mode-map (kbd "l")   (lambda () (interactive) (scroll-left 1)))

  (define-key image-mode-map (kbd "A-h") (lambda () (interactive) (scroll-left 10)))
  (define-key image-mode-map (kbd "A-j") (lambda () (interactive) (scroll-up 10)))
  (define-key image-mode-map (kbd "A-k") (lambda () (interactive) (scroll-down 10)))
  (define-key image-mode-map (kbd "A-l") (lambda () (interactive) (scroll-right 10)))

  (define-key image-mode-map (kbd "H")   'image-previous-file)
  (define-key image-mode-map (kbd "J")   'image-next-file)
  (define-key image-mode-map (kbd "K")   'image-previous-file)
  (define-key image-mode-map (kbd "L")   'image-next-file)

  (define-key image-mode-map (kbd "RET") (lambda ()
                                           (interactive)
                                           (run-associated-program buffer-file-name))))
