;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/vimperator//evil ()
  "Configure `auto-mode-alist' for `vimperator'."
  (setq evil-shift-width 2)
  (evil-local-mode)
  (evil-normal-state))

(defun serika-l/vimperator//buffer-local-variables ()
  "Configure buffer-local variables for `vimperator'."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/vimperator//interface ()
  "Configure interface for `emacs-lisp' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

(defun init ()
  "Configure `vimperator'."
  (serika-c/eg/add-install :type 'download
                           :name 'vimperator
                           :src  "https://raw.githubusercontent.com/mikae/vimperator-mode/master/vimperator-mode.el")

  (serika-c/eg/add-many 'vimperator
                        ("require")
                        (lambda ()
                          (require 'vimperator-mode))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.vimperatorrc\\'" . vimperator-mode))
                          (add-to-list 'auto-mode-alist '("\\.vimperatorrc\\.after\\'" . vimperator-mode))
                          (add-to-list 'auto-mode-alist '("\\.vimp\\'" . vimperator-mode))
                          (add-to-list 'auto-mode-alist '("\\.vimp\\.after\\'" . vimperator-mode)))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/vimperator//evil
                                                  #'serika-l/vimperator//buffer-local-variables
                                                  #'serika-l/vimperator//interface))
                            (func/hook/add 'vimperator-mode-hook
                                               callback)))))
