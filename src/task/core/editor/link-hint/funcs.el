;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `link-hint'."
  (serika-c/eg/add-install :type 'package
                           :name 'link-hint
                           :package-list '(link-hint))

  (serika-c/eg/add-many-by-name 'link-hint
                                ("require")
                                (func/func/requirer link-hint)

                                ("settings")
                                (lambda ()
                                  (setq browse-url-browser-function 'browse-url-firefox))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-z l o" #'link-hint-open-link)
                                  (func/keymap/define-global "C-z l c" #'link-hint-copy-link))))
