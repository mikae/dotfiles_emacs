;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local functions
(defun serika/projectile/ini ()
  "Ini `projectile' project in current directory."
  (interactive)
  (serika/file/create ".projectile")
  (when (equal major-mode 'dired-mode)
    (revert-buffer)))

;; Global configuration
(defun serika/projectile//require ()
  "Require modules for `projectile'."
  (require 'func-file)
  (require 'projectile))

(defun serika/projectile//keymap ()
  "Configure `projectile-map'."
  (setq projectile-mode-map (make-sparse-keymap))

  (define-key projectile-mode-map (kbd "C-p !") 'projectile-run-shell-command-in-root)
  (define-key projectile-mode-map (kbd "C-p &") 'projectile-run-async-shell-command-in-root)

  (define-key projectile-mode-map (kbd "C-p d") 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-p e") 'projectile-edit-dir-locals)
  (define-key projectile-mode-map (kbd "C-p k") 'projectile-kill-buffers)
  (define-key projectile-mode-map (kbd "C-p r") 'projectile-recentf)
  (define-key projectile-mode-map (kbd "C-p t") 'projectile-toggle-between-implementation-and-test)
  (define-key projectile-mode-map (kbd "C-p a") 'projectile-ag)

  (define-key projectile-mode-map (kbd "C-p C") 'projectile-compile-project)
  (define-key projectile-mode-map (kbd "C-p T") 'projectile-test-project)
  (define-key projectile-mode-map (kbd "C-p S") 'projectile-save-project-buffers))

(defun serika/projectile//global-keymap ()
  "Configure global bindings for projectile"
  (global-set-key (kbd "C-x C-p") #'serika/projectile/ini))

;; Init
(defun init ()
  "Configure `projectile'."
  (serika/projectile//require)
  (serika/projectile//keymap)
  (serika/projectile//global-keymap))
