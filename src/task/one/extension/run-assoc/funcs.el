;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `run-assoc'."
  (serika-c/eg/add-install :type 'download
                           :name 'run-assoc
                           :src  "https://raw.githubusercontent.com/mikae/emacswiki.org/master/run-assoc.el")
  (serika-c/eg/add-many-by-name 'run-assoc
                                ("require")
                                (lambda ()
                                  (require 'run-assoc))

                                ("settings")
                                (lambda ()
                                  (setq associated-program-alist
                                        '(("animate" "\\.gif$")
                                          ("smplayer" "\\.webm$")
                                          ((lambda (file)
                                             (browse-url (concat "file:///"
                                                                 (expand-file-name file)))) "\\.html?$"))))))
