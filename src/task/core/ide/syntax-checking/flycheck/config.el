;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

(serika/package/make-sure-installed 'flycheck)

(require 'flycheck)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

(setq flycheck-idle-change-delay 1)

(setq flycheck-error-list-mode-map (make-sparse-keymap))

(define-key flycheck-error-list-mode-map "h" 'evil-backward-char)
(define-key flycheck-error-list-mode-map "j" 'evil-next-visual-line)
(define-key flycheck-error-list-mode-map "k" 'evil-previous-visual-line)
(define-key flycheck-error-list-mode-map "l" 'evil-forward-char)

(global-set-key (kbd "C-, f s") 'flycheck-list-errors)
(global-set-key
 (kbd "C-, f h")
 (lambda ()
   (interactive)
   (serika-buffer-kill-by-major-mode 'flycheck-error-list-mode)))

(provide 'serika-emacs-plugin-configuration-flycheck)
;;; serika-emacs-plugin-configuration-flycheck ends here
