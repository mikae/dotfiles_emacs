;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(cl-defun serika-f/emojify/activate (&key ((:emoji-styles style) '(ascii unicode github)))
  "Activate `emojify-mode' in current buffer.
`emoji-styles' can be:
  - symbols: ascii, unicode, github;
  - list contained symbols described above."
  (setq-local emojify-emoji-styles
              (cond
               ((listp style)
                style)
               ((symbolp style)
                '(style))
               (t (error "Unexpected entity in emojify styles."))))
  (emojify-mode +1))

;; Init
(defun init ()
  "Configure `emojify'."
  (serika-c/eg/add-install :type 'git
                           :name 'emojify
                           :src  "https://github.com/shinkiley/emacs-emojify")

  (serika-c/eg/add-many-by-name 'emojify
    ("require")
    (func/func/require 'emojify)

    ("settings")
    (progn
      (setq emojify-emojis-dir
            (f-join serika-conf-directory
                    "emojis"))
      (setq emojify-program-contexts
            '(comments string)))

    ("keymap")
    (progn
      (func/keymap/save emojify-emoji-keymap)
      (func/keymap/create emojify-emoji-keymap
        "C-c e i" #'emojify-insert-emoji
        "C-c e d" #'emojify-delete-emoji))

    ("post")
    (emojify-download-emoji-maybe)))
