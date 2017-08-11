;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/projectile/ini ()
  "Ini `projectile' project in current directory."
  (interactive)
  (func/file/create ".projectile")
  (when (equal major-mode 'dired-mode)
    (revert-buffer)))

(defun serika-f/projectile/try-activate ()
  "Attempt to activate projectile if in the project."
  (when (projectile-project-p)
    (projectile-mode)))

;; Init
(defun init ()
  "Configure `projectile'."
  (serika-c/eg/add-install :package-list '(projectile)
                           :name         'projectile)

  (serika-c/eg/add-many 'projectile
                        ("require")
                        (lambda ()
                          (require 'projectile))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   projectile-mode-map)
                          (func/keymap/create projectile-mode-map
                                              "C-p !" 'projectile-run-shell-command-in-root
                                              "C-p @" 'projectile-run-async-shell-command-in-root

                                              "C-p d" 'projectile-dired
                                              "C-p e" 'projectile-edit-dir-locals
                                              "C-p k" 'projectile-kill-buffers
                                              "C-p r" 'projectile-recentf
                                              "C-p t" 'projectile-toggle-between-implementation-and-test
                                              "C-p a" 'projectile-ag

                                              "C-p C" 'projectile-compile-project
                                              "C-p T" 'projectile-test-project
                                              "C-p S" 'projectile-save-project-buffers))

                        ("keymap dired")
                        (lambda ()
                          (func/keymap/define dired-mode-map
                                              "n p" #'serika-f/projectile/ini))))
