;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/auto-complete/activate ()
  "Activate `auto-complete' in current buffer."
  (interactive)
  (auto-complete-mode +1))

;; Init
(defun init ()
  "Configure `auto-complete'."
  (serika-c/eg/add-install :type 'git
                           :name 'auto-complete
                           :src "https://github.com/shinkiley/auto-complete")

  (serika-c/eg/add-many-by-name 'auto-complete
    ("require")
    (func/func/require 'auto-complete)

    ("settings")
    (setq ac-delay            0.1
          ac-quick-help-delay 0.1
          ac-auto-start       1
          ac-use-quick-help   t
          ac-menu-height      20
          ac-ignore-case      t
          ac-use-fuzzy        t

          ac-comphist-file   (f-join serika-tmp-directory
                                     "ac-comphist.dat"))

    ("keymap")
    (progn
      (func/keymap/save ac-mode-map)
      (func/keymap/save ac-completing-map)

      (func/keymap/create ac-completing-map
        "A-n" #'ac-stop
        "A-e" #'ac-next
        "A-o" #'ac-complete
        "A-i" #'ac-previous))))
