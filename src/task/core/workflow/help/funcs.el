;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika/help//evil ()
  "Configure `evil' for `help-mode'."
  (evil-local-mode +1)
  (evil-motion-state))

(defun serika/help//keymap ()
  "Configure `help-mode-map'."
  (setq help-mode-map (make-sparse-keymap))

  (define-key help-mode-map (kbd "/") 'evil-search-forward)
  (define-key help-mode-map (kbd "?") 'evil-search-backward)
  (define-key help-mode-map (kbd "n") 'evil-search-next)
  (define-key help-mode-map (kbd "N") 'evil-search-previous))

(defun init ()
  "Configure `help-mode'."
  (serika/help//keymap)

  (add-hook 'help-mode-hook #'serika/help//evil))
