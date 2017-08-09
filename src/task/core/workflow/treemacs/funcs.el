;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/treemacs/create ()
  "Create treemacs window."
  (treemacs-toggle)
  (let ((--window (get-buffer-window "*pu-dummy-file-manager*")))
    (when --window
      (treemacs-toggle)
      (set-window-buffer --window "*Treemacs*"))))

(defun serika-f/treemacs/not-exists-p ()
  "Retutn t if treemacs buffer exists."
  (serika-f/buffer/not-exists-p 'treemacs-mode))

(defun init ()
  "Configure `treemacs'."
  (serika-c/eg/add-install :type         'package
                           :name         'treemacs
                           :package-list '(treemacs))

  (serika-c/eg/add-many 'treemacs
                        ("require")
                        (lambda ()
                          (require 'treemacs))

                        ("settings")
                        (lambda ()
                          (setq treemacs-change-root-without-asking
                                t)
                          (setq treemacs-show-hidden-files
                                t)
                          (add-to-list 'treemacs-ignored-file-predicates
                                       (lambda (filepath)
                                         ;; (string-match "flycheck_.*\\\.el" filepath)
                                         )))

                        ("settings w-purpose")
                        (lambda ()
                          (serika-f/purpose/add 'treemacs-mode
                                                'file-manager))

                        ("keymap")
                        (lambda ()
                          (serika-f/keymap/save   treemacs-mode-map)
                          (serika-f/keymap/create treemacs-mode-map
                                                  "q"   #'treemacs-toggle
                                                  "g"   #'treemacs-refresh
                                                  "RET" #'treemacs-push-button

                                                  "t h" #'treemacs-toggle-show-dotfiles

                                                  "y y" #'treemacs-yank-path-at-point
                                                  "y r" #'treemacs-yank-root

                                                  "n d" #'treemacs-create-dir
                                                  "n f" #'treemacs-create-file

                                                  "d d" #'treemacs-delete
                                                  "d o" #'treemacs-xdg-open

                                                  "A-n" #'treemacs-uproot
                                                  "A-o" #'treemacs-change-root

                                                  "A-e" #'treemacs-next-line
                                                  "A-i" #'treemacs-previous-line))))
