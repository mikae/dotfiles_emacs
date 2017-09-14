;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/xdefaults//evil ()
  "Configure `evil' for `xdefaults'."
  (evil-local-mode)
  (evil-normal-state))

(defun serika-l/xdefaults//buffer-local-variables ()
  "Configure interface for `xdefaults' buffers."
  (setq tab-width        4)
  (setq evil-shift-width 4)
  (setq truncate-lines   t))

(defun serika-l/xdefaults//buffer-local-mappings ()
  "Configure interface for `xdefaults' buffers."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines))

(defun serika-l/xdefaults//interface ()
  "Configure interface for `xdefaults' buffers."
  (setq show-trailing-whitespace 1)

  (rainbow-mode +1)
  (serika-f/linum-relative/activate))

(defun init ()
  "Configure `xdefaults'."
  (serika-c/eg/add-many-by-name 'xdefaults
                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'conf-xdefaults-mode "\\.Xresources\\'"))

                                ("hook")
                                (lambda ()
                                  (dolist (callback (list
                                                     #'serika-l/xdefaults//evil
                                                     #'serika-l/xdefaults//buffer-local-variables
                                                     #'serika-l/xdefaults//buffer-local-mappings

                                                     #'serika-l/xdefaults//interface))
                                    (func/hook/add 'conf-xdefaults-mode-hook
                                                   callback)))))
