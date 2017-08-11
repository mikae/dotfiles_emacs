;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/emojify/activate ()
  "Activate `emojify-mode' in current buffer."
  (emojify-mode +1))

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

                        ("post")
                        (lambda ()
                          (emojify-download-emoji-maybe))))
