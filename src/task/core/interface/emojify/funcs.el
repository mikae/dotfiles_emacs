;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/emojify/activate ()
  "Activate `emojify-mode' in current buffer."
  (emojify-mode +1))

(cl-defmacro serika-f/emojify/create-activator (&key ((:emoji-styles styles) '(ascii unicode github)))
  `(lambda ()
     (emojify-mode +1)
     (setq-local emojify-emoji-styles ,styles)))

;; Init
(defun init ()
  "Configure `emojify'."
  (serika-c/eg/add-install :type 'package
                           :name 'emojify
                           :package-list '(emojify))

  (serika-c/eg/add-many 'emojify
                        ("require")
                        (lambda ()
                          (require 'emojify))

                        ("settings")
                        (lambda ()
                          (setq emojify-emojis-dir
                                (f-join serika-conf-directory
                                        "emojis"))
                          (setq emojify-program-contexts
                                '(comments string)))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save emojify-emoji-keymap)
                          (func/keymap/create emojify-emoji-keymap
                                              "C-c e i" #'emojify-insert-emoji
                                              "C-c e d" #'emojify-delete-emoji))

                        ("post")
                        (lambda ()
                          (emojify-download-emoji-maybe))))
