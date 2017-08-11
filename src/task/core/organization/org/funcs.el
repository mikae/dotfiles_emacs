;;; package --- Summary
;;; Commentary:
;;; Code:

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

;; Init
(defun init ()
  "Configure `org-mode'."
  (serika-c/eg/add-install :package-list '(org)
                           :name         'org)

  (serika-c/eg/add-many 'org
                        ("require")
                        (lambda ()
                          (require 'org))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

                          (setq org-id-locations-file (func/path/join serika-tmp-directory
                                                                      ".org-id-locations"))
                          (setq org-todo-keywords
                                '((sequence "TODO(t)" "DOING(d)" "|" "COMPLETED(c)")
                                  (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")))

                          (setq org-log-done                 t)
                          (setq org-startup-folded           nil)
                          (setq org-startup-truncated        nil)
                          (setq org-structure-template-alist ()))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save   org-mode-map)
                          (func/keymap/create org-mode-map
                                              "<C-tab>"  'org-cycle

                                              
                                              "C-c t i" 'org-insert-todo-heading
                                              "C-c t I" 'org-insert-todo-subheading
                                              "C-c t t" 'org-todo
                                              "C-c t l" 'org-todo-list

                                              
                                              "C-c h i" 'org-insert-heading
                                              "C-c h I" 'org-insert-subheading))

                        ("hook")
                        (lambda ()
                          (add-hook 'org-mode-hook #'serika-l/org//buffer-local-settings)
                          (add-hook 'org-mode-hook #'serika-l/org//interface)
                          (add-hook 'org-mode-hook #'serika-l/org//evil))))
