;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/org/store-note ()
  "Store note."
  (interactive)
  (org-store-log-note))

(defun serika-f/org/insert-item ()
  "Insert item."
  (interactive)
  (when (org-at-item-p)
    (org-insert-heading)))

(defun serika-f/org/insert-checkbox (&optional arg)
  "Insert item."
  (interactive "P")
  (when (org-at-item-p)
    (org-insert-todo-heading arg)))

(defun serika-f/org/toggle-checkbox (&optional arg)
  "Insert item."
  (interactive "P")
  (when (org-at-item-checkbox-p)
    (org-ctrl-c-ctrl-c arg)))

(defun serika-f/org/ctrl-c-ctrl-n ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p)   (call-interactively 'org-promote-subtree))
   ((org-at-item-p)      (call-interactively 'org-outdent-item-tree))
   ((org-at-timestamp-p) (call-interactively 'org-timestamp-down-day))))

(defun serika-f/org/ctrl-c-ctrl-e ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-heading-p)   (call-interactively 'org-forward-heading-same-level))
   ((org-at-item-p)      (call-interactively 'org-shiftdown))
   ((org-at-timestamp-p) (call-interactively 'org-timestamp-down))))

(defun serika-f/org/ctrl-c-ctrl-i ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-heading-p)   (call-interactively 'org-backward-heading-same-level))
   ((org-at-item-p)      (call-interactively 'org-shiftup))
   ((org-at-timestamp-p) (call-interactively 'org-timestamp-up))))

(defun serika-f/org/ctrl-c-ctrl-o ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-heading-p)   (call-interactively 'org-demote-subtree))
   ((org-at-item-p)      (call-interactively 'org-indent-item-tree))
   ((org-at-timestamp-p) (call-interactively 'org-timestamp-up-day))))

(defun serika-f/org/ctrl-c-ctrl-shift-n ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-promote))
   ((org-at-item-p)    (call-interactively 'org-outdent-item))))

(defun serika-f/org/ctrl-c-ctrl-shift-e ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p)    (call-interactively 'org-move-item-down))))

(defun serika-f/org/ctrl-c-ctrl-shift-i ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p)    (call-interactively 'org-move-item-up))))

(defun serika-f/org/ctrl-c-ctrl-shift-o ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-demote))
   ((org-at-item-p)    (call-interactively 'org-indent-item))))

;; Setup buffer
(defun serika-f/org//setup-buffer ()
  "Setup `org' buffer."
  (setq truncate-lines nil)
  (serika-f/linum-relative/activate)
  (serika-f/settings/show-trailing-whitespaces)
  (serika-f/evil/activate :evil-state       'normal
                          :evil-shift-width 4)
  ;; (serika-f/aggressive-indent/activate)
  (serika-f/smartparens/activate))

;; Init
(defun init ()
  "Configure `org-mode'."
  (serika-c/eg/add-install :type 'git
                           :name 'org
                           :src  "git://orgmode.org/org-mode.git"
                           :post-hook "make")

  (serika-c/eg/add-many-by-name
   'org
   ("require")
   (lambda ()
     (require 'org)
     (require 'org-capture))

   ("settings")
   (lambda ()
     ;;`auto-mode-alist'
     (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

     ;; `directories'
     (setq org-directory          (f-join (func/system/user-home)
                                          "org")
           org-id-locations-file  (f-join org-directory
                                          ".data"
                                          "org-id-locations")
           org-archive-location   (f-join org-directory
                                          ".data"
                                          "archive"
                                          "%s_archived::")
           org-default-notes-file (f-join org-directory
                                          "notes.org")
           org-agenda-files       (list (f-join org-directory
                                                ".data")
                                        (f-join org-directory
                                                "notes.org")
                                        (f-join org-directory
                                                "gtd")
                                        (f-join org-directory
                                                "thoughts")))

     ;; relative links
     (setq org-link-file-path-type 'relative)

     ;; `startup'
     (setq org-log-done         'note
           org-startup-folded    nil
           org-startup-truncated nil)

     ;; `templates'
     (setq org-structure-template-alist
           '(("e"
              "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE"
              "<example>\n?\n</example>")
             ("r"
              "#+BEGIN_RULE\n?\n#+END_RULE"
              "<div class=\"rule\">\n?\n</div>")
             ("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
             ("js" "#+BEGIN_SRC js\n?\n#+END_SRC")
             ("ps" ":PROPERTIES:\n:header-args: :results silent\n:END:")))

     ;; `org-babel'
     (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                              (js . t)))
     (setq org-confirm-babel-evaluate nil)

     ;; `todos'
     (setq org-todo-keywords
           '((sequence "TODO(t)" "ACCEPTED(a)" "|" "DONE(d!)"  "CANCELLED(c@)")
             (sequence "BUG(b)"  "CONFIRMED(c)"   "|" "FIXED(f!)" )))

     (setq org-enforce-todo-dependencies          t
           org-enforce-todo-checkbox-dependencies t)

     (setq org-capture-templates
           `(("t" "Todo" entry (file+headline ,(f-join org-directory
                                                       "thoughts"
                                                       "thoughts.org")
                                              "Tasks")
              "* TODO %?\n %i\n %a")))

     (setq org-tag-alist '()))

   ;; todo: fix it
   ;; ("settings aggressive-indent")
   ;; (lambda ()
   ;;   (add-to-list 'aggressive-indent-dont-indent-if
   ;;                '(and (derived-mode-p 'org-mode)
   ;;                      (let ((item (org-element-at-point)))
   ;;                        (and item
   ;;                             (eq 'src-block
   ;;                                 (car item)))))))

   ;; ("settings smartparens")
   ;; (lambda ()
   ;;   ;; todo: make it smarter
   ;;   (sp-local-pair 'org-mode "*" "*"
   ;;                  :unless '(sp-point-at-bol-p))
   ;;   (sp-local-pair 'org-mode "/" "/")
   ;;   (sp-local-pair 'org-mode "_" "_")
   ;;   (sp-local-pair 'org-mode "=" "=")
   ;;   (sp-local-pair 'org-mode "~" "~")
   ;;   (sp-local-pair 'org-mode "+" "+"))

   ("hook")
   (lambda ()
     (add-hook 'org-mode-hook #'serika-f/org//setup-buffer))

   ("global-keymap")
   (lambda ()
     (func/keymap/define-global "C-x <C-o> C-a" #'org-agenda)))

  (serika-c/eg/add-many-by-parents
   ("keymap org")
   'outline
   (lambda ()
     (func/keymap/save   outline-mode-map)
     (func/keymap/create outline-mode-map))

   'org
   (lambda ()
     (func/keymap/save org-mode-map)
     (func/keymap/create org-mode-map
                         ;; Cycling
                         "TAB"       #'org-cycle
                         "<C-tab>"   #'org-global-cycle
                         "RET"       #'org-open-at-point

                         ;; Movements
                         "C-c C-n"   #'serika-f/org/ctrl-c-ctrl-n
                         "C-c C-e"   #'serika-f/org/ctrl-c-ctrl-e
                         "C-c C-i"   #'serika-f/org/ctrl-c-ctrl-i
                         "C-c <C-o>" #'serika-f/org/ctrl-c-ctrl-o
                         "C-c C-u"   #'outline-up-heading

                         "C-c C-S-n" #'serika-f/org/ctrl-c-ctrl-shift-n
                         "C-c C-S-e" #'serika-f/org/ctrl-c-ctrl-shift-e
                         "C-c C-S-i" #'serika-f/org/ctrl-c-ctrl-shift-i
                         "C-c C-S-o" #'serika-f/org/ctrl-c-ctrl-shift-o

                         ;; Headings
                         "C-c C-z h" #'org-insert-heading
                         "C-c C-z H" #'org-insert-subheading

                         ;; Todo
                         "C-c C-z t" #'org-insert-todo-heading
                         "C-c C-z T" #'org-insert-todo-subheading

                         ;; Headings & Todo
                         "C-c h z"   #'org-cut-subtree
                         "C-c h x"   #'org-copy-subtree
                         "C-c h c"   #'org-paste-subtree
                         "C-c h v"   #'org-mark-subtree
                         "C-c h t"   #'org-todo
                         "C-c h p"   #'org-priority

                         ;; Items
                         "C-c C-z i" #'serika-f/org/insert-item

                         ;; Checkbox
                         "C-c C-z v" #'serika-f/org/insert-checkbox
                         "C-c v t"   #'serika-f/org/toggle-checkbox

                         ;; Tables
                         "C-c C-z |" #'org-table-create-with-table.el
                         "C-c t c"   #'org-edit-special

                         ;; Links
                         "C-c C-z l" #'org-insert-link

                         ;; Tags
                         "C-c C-z :" #'org-set-tags-command
                         "C-c : s"   #'org-tags-view

                         ;; Timestamps
                         "C-c C-z ," #'org-time-stamp
                         "C-c C-z ." #'org-time-stamp-inactive

                         ;; Clocks
                         "C-c C-z c d" #'org-deadline
                         "C-c C-z c s" #'org-schedule
                         "C-c C-z c i" #'org-clock-in
                         "C-c C-z c o" #'org-clock-out
                         "C-c c e"     #'org-evaluate-time-range

                         ;; Capture
                         "C-c C-c c" #'org-capture
                         ;; todo remap org-capture-mode-map
                         ;; "C-c C-c f" #'org-capture-finalize

                         ;; Babel execute
                         "C-c C-c e" #'org-babel-execute-src-block

                         ;; Refile
                         "C-c C-c r" #'org-refile

                         ;; Properties
                         "C-c C-z p" #'org-set-property
                         "C-c p k"   #'org-delete-property
                         "C-c p o"   #'org-property-next-allowed-value
                         "C-c p n"   #'org-property-previous-allowed-value

                         ;; Agenda
                         "C-c a a"   #'org-agenda
                         "C-c a t"   #'org-agenda-todo
                         "C-c a c"   #'org-cycle-agenda-files
                         "C-c a n"   #'org-agenda-file-to-front
                         "C-c a d"   #'org-remove-file

                         ;; Notes
                         "C-c n s"   #'serika-f/org/store-note))))
