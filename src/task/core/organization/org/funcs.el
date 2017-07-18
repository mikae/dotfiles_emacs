;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/org//require ()
  "Require modules for `org'."
  (require 'func-path)
  (require 'org))

(defun serika-g/org//auto-mode-alist ()
  "Configure `auto-mode-alist' for `org-mode' buffers."
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(defun serika-g/org//settings ()
  "Configure `org-mode' settings."
  (setq org-id-locations-file (serika-f/path/join serika-tmp-directory
                                                ".org-id-locations"))

  (setq org-log-done               t)

  (setq org-confirm-babel-evaluate nil)

  (setq org-src-fontify-natively   t)

  (setq org-startup-folded         nil)

  (setq org-startup-truncated      nil)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp         . t)
                                 (js                 . t)))

  (setq org-structure-template-alist ())

  (add-to-list 'org-structure-template-alist '("s"  "#+BEGIN_SRC ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
  (add-to-list 'org-structure-template-alist '("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>")))

(defun serika-g/org//keymap ()
  "Configure `org-mode' keymap."
  (setq org-mode-map (make-sparse-keymap))

  (define-key org-mode-map (kbd "<C-tab>")  'org-cycle)

  (define-key org-mode-map (kbd "<C-return>")   'org-insert-todo-heading)
  (define-key org-mode-map (kbd "<C-S-return>") 'org-insert-todo-subheading))

;; Local
(defun serika-l/org//buffer-local-settings ()
  "Configure buffer-local settings for `org-mode' buffers."
  (setq truncate-lines nil))

(defun serika-l/org//interface ()
  "Configure interface for `org-mode' buffers."
  (linum-mode              +1)

  (prettify-symbols-mode   +1)
  (setq prettify-symbols-alist ())

  (setq show-trailing-whitespace t))

(defun serika-l/org//evil ()
  "Configure `evil' for `org-mode' buffers."
  (evil-local-mode         +1)
  (evil-normal-state))

(defun init ()
  "Configure `org-mode'."
  (serika-c/eg/add-install :package-list '(org)
                           :name         'org)

  (serika-c/eg/add :parents '("settings")
                   :name    'org
                   :func    #'serika-g/org//settings)

  (serika-c/eg/add :parents '("settings org")
                   :name    'auto-mode-alist
                   :func    #'serika-g/org//auto-mode-alist)

  (serika-c/eg/add :parents '("keymap")
                   :name    'org
                   :func    #'serika-g/org//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'org
                   :func    (lambda ()
                              (add-hook 'org-mode-hook #'serika-l/org//bufer-local-settings)
                              (add-hook 'org-mode-hook #'serika-l/org//interface)
                              (add-hook 'org-mode-hook #'serika-l/org//evil))))
