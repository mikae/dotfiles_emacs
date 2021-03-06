;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/image/scroll-up (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-up 1))

(defun serika-f/image/scroll-right (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-right 1))

(defun serika-f/image/scroll-bottom (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-bottom 1))

(defun serika-f/image/scroll-left (&optional count)
  "Scroll right."
  (interactive "p")
  (scroll-left 1))

;; Global
(defun init ()
  "Configure `image-mode'."
  (serika-c/eg/add-many-by-name 'image
    ("settings")
    (progn
      ;; `load-path'
      (serika-f/settings/register-ft 'image-mode
                                     "\\.bmp\\'"
                                     "\\.jpg\\'"
                                     "\\.jpeg\\'"
                                     "\\.png\\'")

      (setq image-animate-loop t))

    ("keymap")
    (progn
      (func/keymap/save   image-mode-map)
      (func/keymap/create image-mode-map
        "q"     #'func/buffer/kill-current

        "n"     #'serika-f/image/scroll-left
        "e"     #'serika-f/image/scroll-down
        "i"     #'serika-f/image/scroll-up
        "o"     #'serika-f/image/scroll-right

        "A-E"   #'image-next-file
        "A-I"   #'image-previous-file))))
