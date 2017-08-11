;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `visual-steroids'."
  (serika-c/eg/add-install :type 'package
                           :name 'visual-regexp
                           :package-list '(visual-regexp
                                           visual-regexp-steroids))
  (serika-c/eg/add-many 'visual-regexp
                        ("require")
                        (lambda ()
                          (require 'visual-regexp)
                          (require 'visual-regexp-steroids))

                        ("keymap evil")
                        (lambda ()
                          (define-key evil-normal-state-map (kbd "A-2") #'vr/isearch-forward)
                          (define-key evil-normal-state-map (kbd "A-@") #'vr/isearch-backward)
                          (define-key evil-normal-state-map (kbd "A-3") #'vr/replace)
                          (define-key evil-normal-state-map (kbd "A-#") #'vr/query-replace))))
