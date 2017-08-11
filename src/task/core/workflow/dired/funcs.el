;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/dired/open-this-directory ()
  "Opens dired in this directory."
  (interactive)
  (dired default-directory))

(defun serika-f/dired/create-directory ()
  (interactive)
  (call-interactively 'dired-create-directory)
  (revert-buffer))

(defun serika-f/dired/header-length ()
  "Return the length of the header in current dired buffer."
  2)

(defun serika-f/dired/footer-length ()
  "Return the length of the header in current dired buffer."
  1)

(defun serika-f/dired/omit-header ()
  "When cursor at the beginning of the buffer move it to the beginning of actual content."
  (forward-line (max (- (serika-f/dired/header-length)
                     (1- (line-number-at-pos)))
                  0)))

(defun serika-f/dired/omit-footer ()
  "When cursor at the end of the buffer move it to the end of actual content."
  (forward-line (min 0
                     (- 1
                        (max 0
                             (- (line-number-at-pos)
                                (- (count-lines (point-min) (point-max))
                                   (serika-f/dired/footer-length))
                                ))))))

(defun serika-f/dired/move-to-beginning ()
  "Move cursor to the beginning of actual content."
  (interactive)
  (evil-goto-first-line)
  (serika-f/dired/omit-header)
  (dired-move-to-filename))

(defun serika-f/dired/move-to-end ()
  "Move cursor to the end of actual content."
  (interactive)
  (evil-goto-line)
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/move-to-window-top ()
  "Move cursor to the top of the displayed content, except footer."
  (interactive)
  (evil-window-top)
  (serika-f/dired/omit-header)
  (dired-move-to-filename))

(defun serika-f/dired/move-to-window-middle ()
  "Move cursor to the middle of the displayed content, except header and footer."
  (interactive)
  (evil-window-middle)
  (serika-f/dired/omit-header)
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/move-to-window-bottom ()
  "Move cursor to the bottom of the displayed content, except footer."
  (interactive)
  (evil-window-bottom)
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/scroll-page-down (&optional count)
  "Scroll COUNT pages down with footer omitted."
  (interactive "P")
  (evil-scroll-page-down (or count 1))
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/scroll-page-up (&optional count)
  "Scroll COUNT pages up with header omitted."
  (interactive "P")
  (evil-scroll-page-up (or count 1))
  (serika-f/dired/omit-header)
  (dired-move-to-filename))

(defun serika-f/dired/scroll-half-page-down (&optional count)
  "Scroll COUNT halfpages down with footer omitted."
  (interactive "P")
  (evil-scroll-down (or count 1))
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/scroll-half-page-up (&optional count)
  "Scroll COUNT halfpages up with header omitted."
  (interactive "P")
  (evil-scroll-up (or count 1))
  (serika-f/dired/omit-header)
  (dired-move-to-filename))

(defun serika-f/dired/next-visual-line (&optional count)
  "Move cursor to COUNT lines down."
  (interactive "P")
  (evil-next-visual-line (or count 1))
  (serika-f/dired/omit-footer)
  (dired-move-to-filename))

(defun serika-f/dired/previous-visual-line (&optional count)
  "Move cursor to COUNT lines down."
  (interactive "P")
  (evil-previous-visual-line (or count 1))
  (serika-f/dired/omit-header)
  (dired-move-to-filename))

(defun serika-f/dired/shn-split ()
  "Split selected files using `shntool' utility"
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (= 2 (length marked-files))
        (progn
          (serika-f/shn/split (first marked-files) (second marked-files))
          (dired-unmark-all-marks)
          (revert-buffer t t))
      (error "2 marked files are required for `shn-split'"))))

;; Local
(defun serika-l/dired//buffer-local-settings ()
  "Configure `dired-mode' buffers."
  ;; Enable line truncations
  (setq truncate-lines t)

  ;; auto revert mode
  (serika-f/settings/auto-revert-mode)

  ;; Omit some files
  (dired-omit-mode  +1))

;; Init
(defun init ()
  "Configure `dired'."
  (serika-c/eg/add-many 'dired
                        ("require")
                        (lambda ()
                          (require 'dired)
                          (require 'dired-x)
                          (require 'evil))

                        ("settings")
                        (lambda ()
                          (setq dired-recursive-deletes 'always
                                dired-dwim-target       t)

                          ;; Omit some files with patterns
                          (setq dired-omit-files
                                (string-join (list "^\\.?#"
                                                   "^\\.$"
                                                   "^\\.\\.$"
                                                   "^flycheck_\.+\\.el$")
                                             "\\|"))
                          (setq dired-omit-verbose nil))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save dired-mode-map)
                          (func/keymap/create dired-mode-map
                                              "d d"   #'dired-do-delete
                                              "d r"   #'dired-do-rename
                                              "d c"   #'dired-do-copy

                                              "* m"   #'dired-mark
                                              "* u"   #'dired-unmark

                                              "n d"   #'serika-f/dired/create-directory
                                              "n f"   #'helm-find-files
                                              "n h"   #'dired-do-hardlink
                                              "n s"   #'dired-do-symlink

                                              "c o"   #'dired-do-chown
                                              "c g"   #'dired-do-chgrp

                                              "t o"   (func/func/create-minor-mode-toggler dired-omit-mode)

                                              "q"     (lambda ()
                                                        (interactive)
                                                        (func/buffer/kill-by-major-mode 'dired-mode))

                                              
                                              "A-n"     #'dired-up-directory
                                              "A-e"     #'serika-f/dired/next-visual-line
                                              "A-o"     #'dired-find-file
                                              "A-i"     #'serika-f/dired/previous-visual-line

                                              
                                              "A-1"     #'evil-search-forward
                                              "A-2"     #'evil-search-backward
                                              "A-z"     #'evil-search-next
                                              "A-Z"     #'evil-search-previous

                                              "A-t"     #'serika-f/dired/move-to-beginning
                                              "A-T"     #'serika-f/dired/move-to-end

                                              "A-I"     #'serika-f/dired/move-to-window-top
                                              "A-E"     #'serika-f/dired/move-to-window-bottom
                                              "A-\""    #'serika-f/dired/move-to-window-middle

                                              "A-p"     #'serika-f/dired/scroll-page-down
                                              "A-P"     #'serika-f/dired/scroll-page-up

                                              "RET"     #'dired-run-associated-program
                                              "C-x C-s" #'ignore)
                          (func/keymap/bind-digits dired-mode-map #'digit-argument))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "C-x d" #'serika-f/dired/open-this-directory))

                        ("hook")
                        (lambda ()
                          (add-hook 'dired-mode-hook #'serika-l/dired//buffer-local-settings))))
