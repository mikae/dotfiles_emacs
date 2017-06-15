;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'magit)

(defun serika/magit//keybinding-lambda (target-map)
  "Bind keybinding lambda."
  (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ))
    (define-key target-map (kbd key) 'digit-argument))

  (define-key target-map (kbd "h")    'evil-backward-char)
  (define-key target-map (kbd "j")    'evil-next-visual-line)
  (define-key target-map (kbd "k")    'evil-previous-visual-line)
  (define-key target-map (kbd "l")    'evil-forward-char)

  (define-key target-map (kbd "A-g")  'evil-goto-first-line)
  (define-key target-map (kbd "A-G")  'evil-goto-line)

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
  (define-key target-map (kbd "A-N")  'evil-search-previous))

(defun serika/magit//status-mode-keymap ()
  "Configure keymaps for `magit'."
  (setq magit-status-mode-map (make-sparse-keymap))
  (serika/magit//keybinding-lambda magit-status-mode-map))

(defun init ()
  "Configure `magit'."
  (serika/magit//status-mode-keymap))
