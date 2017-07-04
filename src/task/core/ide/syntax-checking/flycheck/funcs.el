;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/flycheck//require ()
  "Require modules for `flycheck'."
  (require 'func-buffer)
  (require 'flycheck))

(defun serika/flycheck//settings ()
  "Configure `flycheck' settings."
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (setq flycheck-idle-change-delay 1))

(defun serika/flycheck//keymap ()
  "Configure `flycheck' keymaps."
  (setq flycheck-error-list-mode-map (make-sparse-keymap))

  (define-key flycheck-error-list-mode-map "h" 'evil-backward-char)
  (define-key flycheck-error-list-mode-map "j" 'evil-next-visual-line)
  (define-key flycheck-error-list-mode-map "k" 'evil-previous-visual-line)
  (define-key flycheck-error-list-mode-map "l" 'evil-forward-char))

(defun serika/flycheck//global-keymap ()
  "Configure global keymap for using `flycheck'."
  (global-set-key (kbd "C-, f s") 'flycheck-list-errors)
  (global-set-key
   (kbd "C-, f h")
   (lambda ()
     (interactive)
     (serika/buffer/kill-by-major-mode 'flycheck-error-list-mode))))

(defun init ()
  "Configure `flycheck'."
  (serika/flycheck//require)
  (serika/flycheck//settings)
  (serika/flycheck//keymap)
  (serika/flycheck//global-keymap))
