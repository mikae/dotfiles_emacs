;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/org/table-up-field ()
  (interactive)
  (message "Not implemented :("))

(defun serika-f/org/table-down-field ()
  (interactive)
  (message "Not implemented :("))

(defun serika-f/org/table-move-left ()
  (interactive)
  (message "Not implemented :("))

(defun serika-f/org/table-move-down ()
  (interactive)
  (message "Not implemented :("))

(defun serika-f/org/table-move-up ()
  (interactive)
  (message "Not implemented :("))

(defun serika-f/org/table-move-right ()
  (interactive)
  (message "Not implemented :("))

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
   ((org-at-table-p)   (call-interactively 'org-table-previous-field))
   ((org-at-heading-p) (call-interactively 'org-promote-subtree))
   ((org-at-item-p)    (call-interactively 'org-outdent-item-tree))))

(defun serika-f/org/ctrl-c-ctrl-e ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org//table-up-field))
   ((org-at-heading-p) (call-interactively 'org-forward-heading-same-level))
   ((org-at-item-p)    (call-interactively 'org-shiftdown))))

(defun serika-f/org/ctrl-c-ctrl-i ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org//table-down-field))
   ((org-at-heading-p) (call-interactively 'org-backward-heading-same-level))
   ((org-at-item-p)    (call-interactively 'org-shiftup))))

(defun serika-f/org/ctrl-c-ctrl-o ()
  "When cursor at:
"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'org-table-next-field))
   ((org-at-heading-p) (call-interactively 'org-demote-subtree))
   ((org-at-item-p)    (call-interactively 'org-indent-item-tree))))

(defun serika-f/org/ctrl-c-ctrl-shift-n ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org/table-move-left))
   ((org-at-heading-p) (call-interactively 'org-promote))
   ((org-at-item-p)    (call-interactively 'org-outdent-item))))

(defun serika-f/org/ctrl-c-ctrl-shift-e ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org/table-move-down))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p)    (call-interactively 'org-move-item-down))))

(defun serika-f/org/ctrl-c-ctrl-shift-i ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org/table-move-up))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p)    (call-interactively 'org-move-item-up))))

(defun serika-f/org/ctrl-c-ctrl-shift-o ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-table-p)   (call-interactively 'serika-f/org/table-move-right))
   ((org-at-heading-p) (call-interactively 'org-demote))
   ((org-at-item-p)    (call-interactively 'org-indent-item))))

;;
(defun serika-f/org//setup-buffer ()
  "Setup `org' buffer."
  (setq truncate-lines nil)
  (serika-f/linum-relative/activate)
  (serika-f/settings/show-trailing-whitespaces)
  (serika-f/evil/activate :evil-state       'normal
                          :evil-shift-width 4)
  (serika-f/aggressive-indent/activate))

;; Init
(defun init ()
  "Configure `org-mode'."
  (serika-c/eg/add-install :package-list '(org)
                           :name         'org)

  (serika-c/eg/add-many-by-name 'org
                        ("require")
                        (lambda ()
                          (require 'org)
                          (require 'org-capture))

                        ("settings")
                        (lambda ()
                          ;;`auto-mode-alist'
                          (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

                          ;; `directories'
                          (setq org-directory (f-join (func/system/user-home)
                                                      "org")
                                org-id-locations-file (f-join org-directory
                                                              ".hidden"
                                                              ".org-id-locations")
                                org-default-notes-file (f-join org-directory
                                                               "notes.org")
                                org-archive-location (f-join org-directory
                                                             ".archive"
                                                             "%s_archived::"))
                          ;; `settings'
                          (setq org-log-done                'note
                                org-startup-folded           nil
                                org-startup-truncated        nil
                                org-structure-template-alist ()
                                org-log-into-drawer          t)

                          (setq org-todo-keywords
                                '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))

                          (setq org-capture-templates
                                `(("t"
                                   "Todo"
                                   entry
                                   (file+headline ,(f-join org-directory
                                                           "thoughts"
                                                           "gtd.org")
                                                  "Tasks")
                                   "* TODO %?\n %i\n %a")))

                          (setq org-agenda-files (list org-directory))
                          )

                        ("keymap")
                        (lambda ()
                          (func/keymap/save org-mode-map)
                          (func/keymap/create org-mode-map
                                              ;; Cycling
                                              "TAB"       #'org-cycle
                                              "S-TAB"     #'org-cycle

                                              ;; Movements
                                              "C-c C-n"   #'serika-f/org/ctrl-c-ctrl-n
                                              "C-c C-e"   #'serika-f/org/ctrl-c-ctrl-e
                                              "C-c C-i"   #'serika-f/org/ctrl-c-ctrl-i
                                              "C-c <C-o>" #'serika-f/org/ctrl-c-ctrl-o

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

                                              "C-c t t"   #'org-todo
                                              "C-c t p"   #'org-priority

                                              ;; Headings & Todo
                                              "C-c h z"   #'org-cut-subtree
                                              "C-c h x"   #'org-copy-subtree
                                              "C-c h c"   #'org-paste-subtree
                                              "C-c h v"   #'org-mark-subtree

                                              ;; Items
                                              "C-c C-z i" #'serika-f/org/insert-item

                                              ;; Checkbox
                                              "C-c C-z v" #'serika-f/org/insert-checkbox
                                              "C-c C-c t"   #'serika-f/org/toggle-checkbox

                                              ;; Tables
                                              "C-c C-z |" #'org-table-create
                                              "C-c n r"   #'org-table-insert-row
                                              "C-c n c"   #'org-table-insert-column
                                              "C-c n -"   #'org-table-insert-hline
                                              "C-c n _"   #'org-table-hline-and-move
                                              "C-c k r"   #'org-table-kill-row
                                              "C-c s t"   #'org-table-sort-lines

                                              ;; Links
                                              "C-c C-z l" #'org-insert-link

                                              ;; Tags
                                              "C-c C-z :" #'org-set-tags-command

                                              ;; Properties
                                              "C-c C-z s" #'org-set-property-and-value

                                              ;; Timestamps
                                              "C-c C-z ," #'org-time-stamp
                                              "C-c C-z ." #'org-time-stamp-inactive
                                              "C-c <"     #'org-timestamp-down
                                              "C-c >"     #'org-timestamp-up

                                              "C-c C-z d"   #'org-deadline
                                              "C-c C-z s"   #'org-scheduled
                                              "C-c C-z c i" #'org-clock-in
                                              "C-c C-z c o" #'org-clock-out

                                              ;; Capture
                                              "C-c C-c c" #'org-capture

                                              ;; Refile
                                              "C-c C-c r" #'org-refile

                                              ;; Agenda
                                              "C-c C-a a" #'org-agenda-file-to-front
                                              "C-c C-a k" #'org-remove-file
                                              "C-c C-a c" #'org-cycle-agenda-files

                                              ;; Misc
                                              "C-t = t"   #'org-indent-item-tree
                                              "C-t = l"   #'org-indent-line
                                              "C-t = i"   #'org-indent-item
                                              "C-t = r"   #'org-indent-region

                                              "RET"       #'org-open-at-point
                                              )
                          )

                        ("hook")
                        (lambda ()
                          (add-hook 'org-mode-hook #'serika-f/org//setup-buffer))))
