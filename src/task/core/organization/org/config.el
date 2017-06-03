;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)
(require 'func-path)

(serika/package/make-sure-installed 'org)

(require 'org)

(setq org-id-locations-file (serika/path/join serika-tmp-directory
                                              ".org-id-locations"))

(setq org-log-done               t)

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively   t)

(setq org-startup-folded         nil)

(setq org-startup-truncated      t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp         . t)
                               (js                 . t)))

(setq org-structure-template-alist ())

(add-to-list 'org-structure-template-alist '("s"  "#+BEGIN_SRC ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
(add-to-list 'org-structure-template-alist '("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>"))

(evil-set-initial-state 'org-mode 'normal)

(setq org-mode-map (make-sparse-keymap))

(define-key org-mode-map (kbd "<C-tab>")  'org-cycle)

(define-key org-mode-map (kbd "<C-return>")   'org-insert-todo-heading)
(define-key org-mode-map (kbd "<C-S-return>") 'org-insert-todo-subheading)

(add-hook 'org-mode-hook (lambda ()
                           (linum-mode              1)
                           (prettify-symbols-mode   1)
                            (when (boundp 'ac-sources)
                                  (setq ac-sources ()))
                            (setq prettify-symbols-alist ())
                           ()))
