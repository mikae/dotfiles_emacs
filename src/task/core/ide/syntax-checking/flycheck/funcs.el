;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/flycheck//require ()
  "Require modules for `flycheck'."
  (require 'func-buffer)
  (require 'flycheck))

(defun serika-g/flycheck//settings ()
  "Configure `flycheck' settings."
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (setq flycheck-idle-change-delay 1))

(defun serika-g/flycheck//keymap ()
  "Configure `flycheck' keymaps."
  (setq flycheck-error-list-mode-map (make-sparse-keymap))

  (define-key flycheck-error-list-mode-map "h" 'evil-backward-char)
  (define-key flycheck-error-list-mode-map "j" 'evil-next-visual-line)
  (define-key flycheck-error-list-mode-map "k" 'evil-previous-visual-line)
  (define-key flycheck-error-list-mode-map "l" 'evil-forward-char))

(defun serika-g/flycheck//global-keymap ()
  "Configure global keymap for using `flycheck'."
  (global-set-key (kbd "C-, f s") 'flycheck-list-errors)
  (global-set-key
   (kbd "C-, f h")
   (lambda ()
     (interactive)
     (serika-f/buffer/kill-by-major-mode 'flycheck-error-list-mode))))

;; Init
(defun init ()
  "Configure `flycheck'."
  (serika-g/flycheck//require)
  (serika-g/flycheck//settings)
  (serika-g/flycheck//keymap)
  (serika-g/flycheck//global-keymap))
