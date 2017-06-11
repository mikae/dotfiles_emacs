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
