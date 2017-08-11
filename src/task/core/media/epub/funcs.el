;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Todo:
;;; - setup org-mode integration

;; Init
(defun init ()
  "Configure `epub'."
  (serika-c/eg/add-install :type    'git
                           :name    'ereader
                           :src     "https://github.com/mikae/emacs-ereader"
                           :parents '("install ereader"))

  (serika-c/eg/add-install :package-list '(xml+ s)
                           :name         'packages
                           :parents      '("install ereader"))

  (serika-c/eg/add-many 'epub
                        ("require")
                        (lambda ()
                          (require 'ereader))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.epub\\'" . ereader-mode)))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   ereader-mode-map)
                          (func/keymap/create ereader-mode-map
                                              ;; Searching
                                              "A-1"   #'evil-search-forward
                                              "A-2"   #'evil-search-backward
                                              "A-z"   #'evil-search-next
                                              "A-Z"   #'evil-search-previous

                                              
                                              "A-e"   #'evil-scroll-down
                                              "A-i"   #'evil-scroll-up
                                              "A-p"   #'evil-scroll-page-down
                                              "A-P"   #'evil-scroll-page-up

                                              
                                              "C-c A" #'ereader-hide-annotation
                                              "C-c G" #'ereader-goto-chapter
                                              "C-c M" #'ereader-hide-all-annotations
                                              "C-c R" #'ereader-load-annotations
                                              "C-c a" #'ereader-show-annotation
                                              "C-c c" #'ereader-message-chapter
                                              "C-c g" #'ereader-goto-chapter
                                              "C-c m" #'ereader-show-all-annotations
                                              ;; "C-c l" 'org-store-link
                                              ))))
