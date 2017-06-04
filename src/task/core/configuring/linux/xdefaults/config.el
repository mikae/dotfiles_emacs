;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-language-xdefaults-configure-evil ()
  "Configure `evil' for `xdefaults'."
  (evil-set-initial-state 'conf-xdefaults-mode 'normal))

(defun serika-language-xdefaults-configure-keymap ()
  "Configure `conf-xdefaults-mode-map'."
  (setq conf-xdefaults-mode-map (make-sparse-keymap)))

(defun serika-language-xdefaults-configure-buffer-local-variables ()
  "Configure interface for `xdefaults' buffers."
  (setq tab-width        4)
  (setq evil-shift-width 4)
  (setq truncate-lines   t))

(defun serika-language-xdefaults-configure-buffer-local-mappings ()
  "Configure interface for `xdefaults' buffers."
  (evil-local-set-key 'normal (kbd "=") 'evil-indent)
  (evil-local-set-key 'normal (kbd "A-/") 'evilnc-comment-or-uncomment-lines))

(defun serika-language-xdefaults-configure-interface ()
  "Configure interface for `xdefaults' buffers."
  (setq show-trailing-whitespace 1)

  (rainbow-mode +1)
  (linum-mode   +1))

;; Configuration
(serika-language-xdefaults-configure-evil)
(serika-language-xdefaults-configure-keymap)

;; Hooks
(add-hook 'conf-xdefaults-mode-hook 'serika-language-xdefaults-configure-buffer-local-variables)
(add-hook 'conf-xdefaults-mode-hook 'serika-language-xdefaults-configure-buffer-local-mappings)

(add-hook 'conf-xdefaults-mode-hook 'serika-language-xdefaults-configure-interface)


(provide 'serika-emacs-language-xdefaults)
;;; serika-emacs-language-xdefaults.el ends here
