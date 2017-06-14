;; Load dired
(require 'dired)
(require 'evil)

(defun serika/dired/open-this-directory ()
  "Opens dired in this directory."
  (interactive)
  (dired default-directory))

(defun serika/dired/header-length ()
  "Return the length of the header in current dired buffer."
  2)

(defun serika/dired/footer-length ()
  "Return the length of the header in current dired buffer."
  1)

(defun serika/dired/omit-header ()
  "When cursor at the beginning of the buffer move it to the beginning of actual content."
  (forward-line (max (- (serika/dired/header-length)
                     (1- (line-number-at-pos)))
                  0)))

(defun serika/dired/omit-footer ()
  "When cursor at the end of the buffer move it to the end of actual content."
  (forward-line (min 0
                     (- 1
                        (max 0
                             (- (line-number-at-pos)
                                (- (count-lines (point-min) (point-max))
                                   (serika/dired/footer-length))
                                ))))))

(defun serika/dired/move-to-beginning ()
  "Move cursor to the beginning of actual content."
  (interactive)
  (evil-goto-first-line)
  (serika/dired/omit-header)
  (dired-move-to-filename))

(defun serika/dired/move-to-end ()
  "Move cursor to the end of actual content."
  (interactive)
  (evil-goto-line)
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/move-to-window-top ()
  "Move cursor to the top of the displayed content, except footer."
  (interactive)
  (evil-window-top)
  (serika/dired/omit-header)
  (dired-move-to-filename))

(defun serika/dired/move-to-window-middle ()
  "Move cursor to the middle of the displayed content, except header and footer."
  (interactive)
  (evil-window-middle)
  (serika/dired/omit-header)
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/move-to-window-bottom ()
  "Move cursor to the bottom of the displayed content, except footer."
  (interactive)
  (evil-window-bottom)
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/scroll-page-down (&optional count)
  "Scroll COUNT pages down with footer omitted."
  (interactive "P")
  (evil-scroll-page-down (or count 1))
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/scroll-page-up (&optional count)
  "Scroll COUNT pages up with header omitted."
  (interactive "P")
  (evil-scroll-page-up (or count 1))
  (serika/dired/omit-header)
  (dired-move-to-filename))

(defun serika/dired/scroll-half-page-down (&optional count)
  "Scroll COUNT halfpages down with footer omitted."
  (interactive "P")
  (evil-scroll-down (or count 1))
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/scroll-half-page-up (&optional count)
  "Scroll COUNT halfpages up with header omitted."
  (interactive "P")
  (evil-scroll-up (or count 1))
  (serika/dired/omit-header)
  (dired-move-to-filename))

(defun serika/dired/next-visual-line (&optional count)
  "Move cursor to COUNT lines down."
  (interactive "P")
  (evil-next-visual-line (or count 1))
  (serika/dired/omit-footer)
  (dired-move-to-filename))

(defun serika/dired/previous-visual-line (&optional count)
  "Move cursor to COUNT lines down."
  (interactive "P")
  (evil-previous-visual-line (or count 1))
  (serika/dired/omit-header)
  (dired-move-to-filename))

(defun serika/dired/shn-split ()
  "Split selected files using `shntool' utility"
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (= 2 (length marked-files))
        (progn
          (serika/shn/split (first marked-files) (second marked-files))
          (dired-unmark-all-marks)
          (revert-buffer t t))
      (error "2 marked files are required for `shn-split'"))))

(defun serika/dired//settings ()
  "Configure `dired-mode' settings."
  (setq dired-recursive-deletes 'always
        dired-dwim-target       t))

(defun serika/dired//keymap ()
  "Configure `dired-mode' keymap."
  (setq dired-mode-map (make-sparse-keymap))

  (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ))
    (define-key dired-mode-map (kbd key) 'digit-argument))

  ;; Dired actions
  (define-key dired-mode-map (kbd "d d")   'dired-do-delete)
  (define-key dired-mode-map (kbd "d r")   'dired-do-rename)
  (define-key dired-mode-map (kbd "d c")   'dired-do-copy)

  (define-key dired-mode-map (kbd "* m")   'dired-mark)
  (define-key dired-mode-map (kbd "* u")   'dired-unmark)

  (define-key dired-mode-map (kbd "n d")   'dired-create-directory)
  (define-key dired-mode-map (kbd "n f")   'helm-find-files)
  (define-key dired-mode-map (kbd "n h")   'dired-do-hardlink)
  (define-key dired-mode-map (kbd "n s")   'dired-do-symlink)

  (define-key dired-mode-map (kbd "c o")   'dired-do-chown)
  (define-key dired-mode-map (kbd "c g")   'dired-do-chgrp)

  (define-key dired-mode-map (kbd "h")     'dired-up-directory)
  (define-key dired-mode-map (kbd "l")     'dired-find-file)

  ;; Kill all dired buffers mapping
  (define-key dired-mode-map (kbd "q")     (lambda ()
                                             (interactive)
                                             (serika/buffer/kill-by-major-mode 'dired-mode)))

  ;; Add search functions
  (define-key dired-mode-map (kbd "/")    'evil-search-forward)
  (define-key dired-mode-map (kbd "?")    'evil-search-backward)
  (define-key dired-mode-map (kbd "A-n")  'evil-search-next)
  (define-key dired-mode-map (kbd "A-N")  'evil-search-previous)

  (define-key dired-mode-map (kbd "j")    'serika/dired/next-visual-line)
  (define-key dired-mode-map (kbd "k")    'serika/dired/previous-visual-line)

  (define-key dired-mode-map (kbd "g")    'serika/dired/move-to-beginning)
  (define-key dired-mode-map (kbd "G")    'serika/dired/move-to-end)

  (define-key dired-mode-map (kbd "A-k")  'serika/dired/move-to-window-top)
  (define-key dired-mode-map (kbd "A-m")  'serika/dired/move-to-window-middle)
  (define-key dired-mode-map (kbd "A-j")  'serika/dired/move-to-window-bottom)

  (define-key dired-mode-map (kbd "A-f")  'serika/dired/scroll-page-down)
  (define-key dired-mode-map (kbd "A-b")  'serika/dired/scroll-page-up)
  (define-key dired-mode-map (kbd "A-d")  'serika/dired/scroll-half-page-down)
  (define-key dired-mode-map (kbd "A-u")  'serika/dired/scroll-half-page-up)

  (define-key dired-mode-map (kbd "RET")  'dired-run-associated-program))

(defun serika/dired//global-keymap ()
  "Configure global keymap to use `dired-mode'."
  (global-set-key (kbd "C-x C-d") 'serika/dired/open-this-directory))

(defun serika/dired//hook ()
  "Configure `dired-mode' buffers."
  ;; Enable auto reverting
  (auto-revert-mode 1)

  ;; Enable line truncations
  (setq truncate-lines t)

  ;; Disable attempt to save dired buffer
  (make-local-variable 'serika-buffer-save-function)
  (setq serika-buffer-save-function 'serika-misc-silence))

(defun init ()
  "Configure `dired'."
  (serika/dired//settings)
  (serika/dired//keymap)
  (serika/dired//global-keymap)

  (add-hook 'dired-mode-hook #'serika/dired//hook))
