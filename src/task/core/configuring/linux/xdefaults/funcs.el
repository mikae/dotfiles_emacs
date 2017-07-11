;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/xdefaults//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.Xresources\\'" . conf-xdefaults-mode)))

(defun serika/xdefaults//keymap ()
  "Configure `conf-xdefaults-mode-map'."
  (setq conf-xdefaults-mode-map (make-sparse-keymap)))

;; Local
(defun serika/xdefaults//evil ()
  "Configure `evil' for `xdefaults'."
  (evil-local-mode)
  (evil-normal-state))

(defun serika/xdefaults//buffer-local-variables ()
  "Configure interface for `xdefaults' buffers."
  (setq tab-width        4)
  (setq evil-shift-width 4)
  (setq truncate-lines   t))

(defun serika/xdefaults//buffer-local-mappings ()
  "Configure interface for `xdefaults' buffers."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines))

(defun serika/xdefaults//interface ()
  "Configure interface for `xdefaults' buffers."
  (setq show-trailing-whitespace 1)

  (rainbow-mode +1)
  (linum-mode   +1))

(defun init ()
  "Configure `xdefaults'."
  (serika/xdefaults//auto-mode-alist)
  (serika/xdefaults//keymap)

  ;; Hooks
  (add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//evil)
  (add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//buffer-local-variables)
  (add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//buffer-local-mappings)

  (add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//interface))
