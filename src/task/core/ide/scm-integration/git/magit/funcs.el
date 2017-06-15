;;; package --- Summary
;;; Commentary:
;;; Code:

;; TODO: fix bug (eval-when-compile (require 'dired-x))
;; it doesn't work, but works after restart
(require 'magit)

(defun serika/magit//keymap ()
  "Configure keymaps for `magit'."
  (let ((keybinding-lambda
         (lambda (target-map)
           (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ))
             (define-key target-map (kbd key) 'digit-argument))

           (define-key target-map (kbd "h")    'evil-backward-char)
           (define-key target-map (kbd "j")    'evil-next-visual-line)
           (define-key target-map (kbd "k")    'evil-previous-visual-line)
           (define-key target-map (kbd "l")    'evil-forward-char)

           (define-key target-map (kbd "A-g")    'evil-goto-first-line)
           (define-key target-map (kbd "A-G")    'evil-goto-line)

           (define-key target-map (kbd "A-k")  'evil-window-top)
           (define-key target-map (kbd "A-m")  'evil-window-middle)
           (define-key target-map (kbd "A-j")  'evil-window-bottom)
           (define-key target-map (kbd "A-h")  'evil-beginning-of-visual-line)
           (define-key target-map (kbd "A-l")  'evil-end-of-visual-line)

           (define-key target-map (kbd "A-f")  'evil-scroll-page-down)
           (define-key target-map (kbd "A-b")  'evil-scroll-page-up)
           (define-key target-map (kbd "A-d")  'evil-scroll-down)
           (define-key target-map (kbd "A-u")  'evil-scroll-up)

           (define-key target-map (kbd "/")    'evil-search-forward)
           (define-key target-map (kbd "?")    'evil-search-backward)
           (define-key target-map (kbd "A-n")  'evil-search-next)
           (define-key target-map (kbd "A-N")  'evil-search-previous)
           )
         ))
    (setq magit-status-mode-map (make-sparse-keymap))
    (funcall keybinding-lambda magit-status-mode-map)))

(defun init ()
  "Configure `magit'."
  (serika/magit//keymap))
