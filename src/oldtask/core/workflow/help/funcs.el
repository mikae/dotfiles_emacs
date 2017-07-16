;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/help//keymap ()
  "Configure `help-mode-map'."
  (setq help-mode-map (make-sparse-keymap))

  (define-key help-mode-map (kbd "/") 'evil-search-forward)
  (define-key help-mode-map (kbd "?") 'evil-search-backward)
  (define-key help-mode-map (kbd "n") 'evil-search-next)
  (define-key help-mode-map (kbd "N") 'evil-search-previous))

;; Local
(defun serika-l/help//evil ()
  "Configure `evil' for `help-mode'."
  (evil-local-mode +1)
  (evil-motion-state))

;; Init
(defun init ()
  "Configure `help-mode'."
  (serika-g/help//keymap)

  (add-hook 'help-mode-hook #'serika-l/help//evil))
