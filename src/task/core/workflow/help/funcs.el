;;; package --- Summary
;;; Commentary:
;;; Code:
(defun serika/help/evil ()
  "Configure `evil' for `help-mode'."
  (evil-set-initial-state 'help-mode 'motion))

(defun serika/help/keymap ()
  "Configure `help-mode-map'."
  (setq help-mode-map (make-sparse-keymap))

  (define-key help-mode-map (kbd "/") 'evil-search-forward)
  (define-key help-mode-map (kbd "?") 'evil-search-backward)
  (define-key help-mode-map (kbd "n") 'evil-search-next)
  (define-key help-mode-map (kbd "N") 'evil-search-previous))
