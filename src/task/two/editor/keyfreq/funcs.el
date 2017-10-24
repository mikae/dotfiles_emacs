;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/keyfreq/activate ()
  "Activate keyfreq."
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;; Init
(defun init ()
  "Configure `keyfreq'."
  (serika-c/eg/add-install :type 'package
                           :name 'keyfreq
                           :package-list '(keyfreq))

  (serika-c/eg/add-many-by-name 'keyfreq
                                ("require")
                                (func/func/require 'keyfreq)

                                ("settings")
                                (lambda ()
                                  (setq keyfreq-excluded-commands
                                        keyfreq-excluded-commands))

                                ;; ("post activate")
                                ;; #'serika-f/keyfreq/activate
                                ))
