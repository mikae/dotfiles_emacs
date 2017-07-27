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
  ;; `filepaths'
  (setq org-id-locations-file (serika-f/path/join serika-tmp-directory
                                                ".org-id-locations"))
  ;; `todo'
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "CANCELED(c)")))

  ;; `settings'
  (setq org-log-done               t)
  (setq org-startup-folded         nil)
  (setq org-startup-truncated      nil)
  (setq org-structure-template-alist ()))

(defun serika-g/org//keymap ()
  "Configure `org-mode' keymap."
  (setq org-mode-map (make-sparse-keymap))

  (define-key org-mode-map (kbd "<C-tab>")  'org-cycle)

  ;; todos
  (define-key org-mode-map (kbd "C-c t i") 'org-insert-todo-heading)
  (define-key org-mode-map (kbd "C-c t I") 'org-insert-todo-subheading)
  (define-key org-mode-map (kbd "C-c t t") 'org-todo)
  (define-key org-mode-map (kbd "C-c t l") 'org-todo-list)
  )

;; Local
(defun serika-l/org//buffer-local-settings ()
  "Configure buffer-local settings for `org-mode' buffers."
  (setq truncate-lines nil))

(defun serika-l/org//interface ()
  "Configure interface for `org-mode' buffers."
  (serika-f/linum-relative/activate)

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
