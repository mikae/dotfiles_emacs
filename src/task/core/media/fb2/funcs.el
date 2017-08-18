;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure fb2"
  (serika-c/eg/add-install :type 'git
                           :name 'fb2-mode
                           :src  "https://github.com/mikae/fb2-mode")

  (serika-c/eg/add-many-by-name 'fb2-mode
                        ("require")
                        (func/func/requirer fb2-mode)

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.fb2\\'" . fb2-mode))))
  )
