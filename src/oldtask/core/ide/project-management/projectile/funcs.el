;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/projectile/ini ()
  "Ini `projectile' project in current directory."
  (interactive)
  (serika-f/file/create ".projectile")
  (when (equal major-mode 'dired-mode)
    (revert-buffer)))

;; Global
(defun serika-g/projectile//require ()
  "Require modules for `projectile'."
  (require 'func-file)
  (require 'projectile))

(defun serika-g/projectile//keymap ()
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

(defun serika-g/projectile//global-keymap ()
  "Configure global bindings for projectile"
  (global-set-key (kbd "C-x p") #'serika-g/projectile/ini))

;; Init
(defun init ()
  "Configure `projectile'."
  (serika-g/projectile//require)
  (serika-g/projectile//keymap)
  (serika-g/projectile//global-keymap))
