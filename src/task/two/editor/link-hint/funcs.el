;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `link-hint'."
  (serika-c/eg/add-install :type 'git
                           :name 'link-hint
                           :src  "https://github.com/shinkiley/link-hint.el")

  (serika-c/eg/add-many-by-name 'link-hint
    ("require")
    (func/func/require 'link-hint)

    ("settings")
    (setq browse-url-browser-function 'browse-url-firefox)

    ("global-keymap")
    (func/keymap/define-global
      "C-z l o" #'link-hint-open-link
      "C-z l c" #'link-hint-copy-link)))
