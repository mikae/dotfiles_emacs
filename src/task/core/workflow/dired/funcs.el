;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Variables
(defvar --serika-dired-omit-mode-hidden-patterns '("^\\\.")
  "Rules for hidden patterns.")

(defvar --serika-dired-omit-mode-omitted-patterns '("^flycheck_\.+\\.el$")
  "Rules for omitted patterns.")

(defvar --serika-dired-omit-mode-bits 0
  "Bitmap for rule selection.
0x - bit of hidden files;
x0 - bit of omitted files.")

;; Private
(defun serika-f/dired//apply-omit-rules ()
  "Creates omit rules for `dired-omit-mode'."
  ;; todo: replace ugly code
  (setq dired-omit-files
        (string-join (cons "^I AM A MAN, WHO WALKS ALONE, AND WHEN I'm WALKING A DARK ROAD$"
                           (append (if (= (logand --serika-dired-omit-mode-bits
                                                  2)
                                          2)
                                       --serika-dired-omit-mode-omitted-patterns
                                     ())
                                   (if (= (logand --serika-dired-omit-mode-bits
                                                  1)
                                          1)
                                       --serika-dired-omit-mode-hidden-patterns
                                     ())))
                     "\\|"))
  (dired-revert))

;; Public
(defun serika-f/dired/toggle-hidden ()
  "Toggles flag of hidden files."
  (interactive)
  (setq --serika-dired-omit-mode-bits
        (logxor --serika-dired-omit-mode-bits
                1))
  (serika-f/dired//apply-omit-rules))

(defun serika-f/dired/toggle-omitted ()
  "Toggles flag of omitted files."
  (interactive)
  (setq --serika-dired-omit-mode-bits
        (logxor --serika-dired-omit-mode-bits
                2))
  (serika-f/dired//apply-omit-rules))

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

(defun serika-f/dired/uncompress-selected ()
  "Uncompress selected files."
  (interactive)
  (let ((--input (read-string "Path: " nil default-directory))
        (--target))
    (setq --target (cond
                    ((string= ""
                              --input)
                     default-directory)
                    ((f-absolute-p --input) --input)
                    (t (f-join default-directory
                               --input))))
    (unless (f-dir-p --target)
      (f-mkdir --target))
    (setq default-directory
          --target)
    (cl-loop for --item in (dired-get-marked-files)
             do (let ((--cmd (format (cond
                                      ((string-match "\\.zip$" --item)
                                       "unzip -d %s \"%s\"")
                                      (t (error "Unsupported archive format.")))
                                     (s-replace "\"" "\\\"" default-directory)
                                     --item)))
                  (when --cmd
                    (shell-command --cmd)
                    (dired-revert))))))

(defun serika-f/dired/create-path-visiter (path)
  "Construct lambda that visits PATH.
If PATH is invalid return nil."
  (let ((--path (if (symbolp path)
                    (symbol-value path)
                  path)))
    (if (f-dir-p --path)
        (lambda ()
          (interactive)
          (dired --path))
      nil)))

;; Local
(defun serika-l/dired//setup-buffer ()
  "Configure `dired-mode' buffers."
  ;; Enable evil in dired mode
  (serika-f/evil/activate :evil-state 'dired)

  ;; Enable line truncations
  (setq truncate-lines t)

  ;; auto revert mode
  (serika-f/settings/auto-revert-mode)

  ;; Omit some files
  (dired-omit-mode  +1)
  (dired-hide-details-mode -1))

;; Init
(defun init ()
  "Configure `dired'."
  (serika-c/eg/add-install :type 'download
                           :name 'dired+
                           :src "https://raw.githubusercontent.com/mikae/emacswiki.org/master/dired%2B.el")

  (serika-c/eg/add-many-by-name 'dired
                                ("require")
                                (lambda ()
                                  (require 'dired)
                                  (require 'dired-x)
                                  (require 'dired+)
                                  (require 'evil))

                                ("settings")
                                (lambda ()
                                  (setq dired-recursive-deletes 'always
                                        dired-dwim-target       t)

                                  ;; Omit some files with patterns
                                  (setq dired-omit-verbose nil)

                                  (setq dired-compress-file-suffixes
                                        '(("\\.zip\\'" ".zip" "unzip"))))

                                ("settings evil")
                                (lambda ()
                                  (evil-define-state dired
                                    "State for dired."
                                    :tag "<Dired>"
                                    :suppress-keymap t))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save dired-mode-map)
                                  (func/keymap/create dired-mode-map))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-x d" #'serika-f/dired/open-this-directory))

                                ("hook")
                                (lambda ()
                                  (add-hook 'dired-mode-hook #'serika-l/dired//setup-buffer)))

  (serika-c/eg/add-many-by-parents ("keymap evil")
                                   'dired
                                   (lambda ()
                                     (func/keymap/create evil-dired-state-map
                                                         "* m"   #'dired-mark
                                                         "* u"   #'dired-unmark
                                                         "* U"   #'dired-unmark-all-marks

                                                         "d d"   #'dired-do-delete
                                                         "d r"   #'dired-do-rename
                                                         "d c"   #'dired-do-copy
                                                         "d l"   #'diredp-list-marked
                                                         "d u"   #'serika-f/dired/uncompress-selected

                                                         "n d"   #'serika-f/dired/create-directory
                                                         "n f"   #'helm-find-files
                                                         "n h"   #'dired-do-hardlink
                                                         "n s"   #'dired-do-symlink

                                                         "c o"   #'dired-do-chown
                                                         "c g"   #'dired-do-chgrp

                                                         "o o"   (serika-f/dired/create-path-visiter org-directory)

                                                         "t h"   #'serika-f/dired/toggle-hidden
                                                         "t o"   #'serika-f/dired/toggle-omitted
                                                         "t d"   (func/func/create-minor-mode-toggler dired-hide-details-mode)

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
                                                         "C-x C-s" #'ignore))))
