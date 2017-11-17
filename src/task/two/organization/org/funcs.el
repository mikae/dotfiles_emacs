;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
(defun serika-f/org/load-railroad-diagrams ()
  ""
  (func/var/ensure-local
   org-html-head-include-scripts t
   org-html-scripts (concat org-html-scripts
                            "<script "
                            "type=\"text/javascript\""
                            "src="
                            (f-join org-directory
                                    "scripts"
                                    "railroad-diagrams"
                                    "railroad-diagrams.js")
                            "></script>")
   org-html-head-extra (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
                               (f-join org-directory
                                       "scripts"
                                       "railroad-diagrams"
                                       "railroad-diagrams.css")))
  )

;; Shadower
(define-minor-mode org-edit-src-shadower-mode
  "Minor mode for shadowing some keys."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-s") #'func/buffer/invoke-save-function)
            (define-key map (kbd "C-x C-c") #'func/buffer/kill)
            map))

;; Some goodies
(defun serika-f/org/create-answer-table (&optional question-count)
  "Create question-answer-correct?-correction table."
  (interactive "P")
  (let ((--question-count (or question-count 1)))
    (org-table-create (format "5x%d" (+ 2 (or question-count 1))))
    ;; header content
    (dolist (--elem '("" "Q" "A" "OK?" "Correction"))
      (org-cycle)
      (insert --elem))
    (org-cycle)
    ;; number of answer
    (dotimes (--i (1- --question-count))
      (insert "#")
      (org-cycle)
      (insert (format "%d)" (1+ --i)))
      (dotimes (_ 4)
        (org-cycle)))
    ;; last row
    (insert "#")
    (org-cycle)
    (insert (format "%d)" --question-count))
    (org-table-insert-hline)
    (forward-line 2)
    (dotimes (_ 4)
      (org-cycle))
    (insert "0%")
    (org-table-align)
    (forward-line)
    (let ((--format (format "#+TBLFM: @%d$4='(format \"%%.2f%%%%\" (* (let ((--s (concat  @2$4..@%d$4))) (/ (s-count-matches \"v\" --s) %.1f)) 100))"
                            (+ 2 --question-count)
                            (+ 1 --question-count)
                            (* --question-count 1.0)
                            )))
      (insert --format))))

(defun serika-f/org/edit-src ()
  "Narrow to src block, excludes BEGIN_SRC and END_SRC."
  (interactive)
  (let ((case-fold-search t)
        (blockp (org-between-regexps-p "^[ \t]*#\\+begin_src.*"
                                       "^[ \t]*#\\+end_src.*")))
    (if blockp
        (let* ((--block-beg      (car blockp))
               (--block-end      (cdr blockp))
               (--beg-end            (save-excursion
                                       (goto-char --block-beg)
                                       (search-forward-regexp "\n")
                                       (point)))
               (--end-beg            (save-excursion
                                       (goto-char --block-end)
                                       (search-backward-regexp "\n")
                                       (point)))
               (--block-beg-line (buffer-substring --block-beg --beg-end))
               (--text           (buffer-substring --beg-end --end-beg))
               (--source-buffer  (current-buffer))
               (--buffer         (generate-new-buffer "edit src"))
               (--org-langs      org-src-lang-modes)
               (--language       (nth 1 (split-string --block-beg-line)))
               (--language-mode  nil))

          ;; Check if the mode from src block is derived mode in `org-src-lang-modes'
          (while (and (null --language-mode)
                      --org-langs)
            (when (string= --language
                           (caar --org-langs))
              (setq --language-mode (intern (concat (symbol-name (cdar --org-langs))
                                                    "-mode"))))
            (setq --org-langs (cdr --org-langs)))

          ;; Otherwise, try to add "-mode" to language from src block
          (unless --language-mode
            (setq --language-mode (intern (concat --language
                                                  "-mode"))))

          (with-current-buffer --buffer
            (switch-to-buffer --buffer)
            (insert --text)
            (when (symbol-function --language-mode)
              (funcall --language-mode))
            (org-edit-src-shadower-mode +1)
            (func/buffer/save-function
             (lambda ()
               (let ((--new-text (buffer-substring (point-min) (point-max))))
                 (with-current-buffer --source-buffer
                   ;; Replace old src block with newer one
                   (kill-region --block-beg
                                --block-end)
                   (goto-char --block-beg)
                   (insert --block-beg-line)
                   (insert --new-text)
                   (insert "\n#+END_SRC")

                   ;; Update src block' beg and end points
                   (forward-line -1)
                   (setq blockp (org-between-regexps-p "^[ \t]*#\\+begin_src.*"
                                                       "^[ \t]*#\\+end_src.*"))
                   (if blockp
                       (setq --block-beg (car blockp)
                             --block-end (cdr blockp))
                     (user-error "Error replacing scr block"))))))))
      (user-error "Not in a src block"))))

;; Org wrapper functions
(defun serika-f/org/recalculate-table ()
  (interactive)
  (when (org-table-p)
    (org-table-recalculate :all)))

(defun serika-f/org/recalculate-row ()
  (interactive)
  (when (org-table-p)
    (org-table-recalculate)))

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
   ((org-at-table-p)     (call-interactively 'org-table-previous-field))
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
   ((org-at-table-p)     (call-interactively 'org-table-next-field))
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
   ((org-at-table-p)   (call-interactively 'org-table-move-column-left))
   ((org-at-item-p)    (call-interactively 'org-outdent-item))))

(defun serika-f/org/ctrl-c-ctrl-shift-e ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-table-p)   (call-interactively 'org-table-move-row-down))
   ((org-at-item-p)    (call-interactively 'org-move-item-down))))

(defun serika-f/org/ctrl-c-ctrl-shift-i ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-table-p)   (call-interactively 'org-table-move-row-up))
   ((org-at-item-p)    (call-interactively 'org-move-item-up))))

(defun serika-f/org/ctrl-c-ctrl-shift-o ()
  "When cursor at:
1)
2)
3)"
  (interactive)
  (cond
   ((org-at-heading-p) (call-interactively 'org-demote))
   ((org-at-table-p)   (call-interactively 'org-table-move-column-right))
   ((org-at-item-p)    (call-interactively 'org-indent-item))))

;; Setup buffer
(defun serika-f/org//setup-buffer ()
  "Setup `org' buffer."
  (when (eq major-mode
            'org-mode)
    (func/var/ensure-local truncate-lines nil)

    (serika-f/evil/activate :evil-state       'normal
                            :evil-shift-width 4)
    ;; (serika-f/auto-indent-mode/activate :style 'conservative)
    (serika-f/smartparens/activate)
    (serika-f/yasnippet/activate)

    (serika-f/prettify-symbols/activate :name "org")
    (serika-f/linum-relative/activate)))

;; Init
(defun init ()
  "Configure `org-mode'."
  (serika-c/eg/add-install :type 'git
                           :name 'org
                           :src  "git://orgmode.org/org-mode.git"
                           :parents '("install org")
                           :extra-path '("lisp")
                           :post-hook "make")

  (dolist (elem '((ob-rust   . "https://github.com/mikae/ob-rust")
                  (ob-fsharp . "https://github.com/mikae/ob-fsharp")))
    (serika-c/eg/add-install :type 'git
                             :name (car elem)
                             :src  (cdr elem)
                             :parents '("install org")))

  (serika-c/eg/add-many-by-name 'org
    ("require")
    (progn
      (require 'org)
      (require 'org-capture)

      (require 'ob-ebnf)
      (require 'ob-haskell)
      (require 'ob-rust)
      (require 'ob-fsharp)
      (require 'ob-shell))

    ("settings")
    (lambda ()
      ;;`auto-mode-alist'
      (serika-f/settings/register-ft 'org-mode "\\.org\\'")

      ;; `directories'
      (setq org-directory          (f-join (f-root)
                                           "home"
                                           "data"
                                           "Data"
                                           "org")
            org-id-locations-file  (f-join org-directory
                                           ".hidden"
                                           "org-id-locations")
            org-archive-location   (f-join org-directory
                                           ".hidden"
                                           "archive"
                                           "%s_archived::")
            org-default-notes-file (f-join org-directory
                                           "notes.org")
            org-agenda-files       (list (f-join org-directory
                                                 "gtd"))
            org-archive-location   (f-join org-directory
                                           "archive"
                                           "archive.org::"))

      ;; relative links
      (setq org-link-file-path-type 'relative)

      ;; `interface'
      (setq org-emphasis-alist '(("*" bold)
                                 ("/" italic)
                                 ("_" underline)
                                 ("=" org-verbatim verbatim)
                                 ("~" org-code verbatim)))

      ;; `latex'
      (setq org-highlight-latex-and-related '(latex entities))

      ;; `html'
      (setq org-html-mathjax-options `((path ,(concat (f-join org-directory
                                                              "scripts"
                                                              "MathJax"
                                                              "MathJax.js")
                                                      "?config=TeX-AMS_HTML"))
                                       (scale "110")
                                       (align "left")))
      (setq org-html-preamble         nil
            org-html-postamble        nil)

      ;; scripts
      (setq org-html-head-include-scripts t
            org-html-scripts              org-html-scripts)

      ;; extra head
      (setq org-html-head-extra "")

      ;; `inline-js'
      (add-to-list 'org-src-lang-modes '("inline-js" . javascript))

      (defvar org-babel-default-header-args:inline-js
        '((:results . "html")
          (:exports . "results")))

      (defun org-babel-execute:inline-js (body _params)
        (format "<script type=\"text/javascript\">\n%s\n</script>" body))

      ;; `startup'
      (setq org-log-done         'note
            org-startup-folded    nil
            org-startup-truncated nil)

      ;; `templates'
      (setq org-structure-template-alist
            '(
              ("ps"    ":PROPERTIES:\n:header-args: :results silent\n:END:")
              ("e"     "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE" "<example>\n?\n</example>")
              ("r"     "#+BEGIN_RULE\n?\n#+END_RULE" "<div class=\"rule\">\n?\n</div>")
              ("s"     "#+BEGIN_SRC ?\n#+END_SRC")
              ("sr"    "#+BEGIN_SRC rust?\n#+END_SRC")
              ("sj"    "#+BEGIN_SRC js\n?\n#+END_SRC")
              ("sh"    "#+BEGIN_SRC haskell\n?\n#+END_SRC")
              ("sss"   "#+BEGIN_SRC shell\n?\n#+END_SRC")
              ("ssb"   "#+BEGIN_SRC bash\n?\n#+END_SRC")
              ("sfs"   "#+BEGIN_SRC fsharp\n?\n#+END_SRC")
              ("sij"   "#+BEGIN_SRC inline-js\n?\n#+END_SRC")
              ("sijrr" "#+BEGIN_SRC inline-js\nComplexDiagram(\n?\n).addTo();\n#+END_SRC")

              ("mpa"   "#+BEGIN_AXIOM\n?\n#+END_AXIOM")
              ("mde"   "#+BEGIN_DEFINITION\n?\n#+END_DEFINITION")
              ("mpp"   "#+BEGIN_PROPOSITION\n?\n#+END_PROPOSITION")
              ("mth"   "#+BEGIN_THEOREM\n?\n#+END_THEOREM")
              ("mpf"   "#+BEGIN_PROOF\n?\n#+END_PROOF")

              ("ebnfy" "#+BEGIN_SRC ebnf :style yacc :file ?\n\n#+END_SRC")
              ("ebnfe" "#+BEGIN_SRC ebnf :style ebnf :file ?\n\n#+END_SRC")
              ("ebnfi" "#+BEGIN_SRC ebnf :style iso-ebnf :file ?\n\n#+END_SRC")))

      ;; `org-babel'
      (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                               (js         . t)
                                                               (python     . t)
                                                               (rust       . t)
                                                               (ebnf       . t)))
      (setq org-confirm-babel-evaluate nil)

      ;; src blocks
      (setq org-src-fontify-natively t)

      ;; `todos'
      (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "|" "CANCELLED(c@/!)")))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "red" :weight bold)
                    ("NEXT" :foreground "blue" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold)
                    ("CANCELLED" :foreground "forest green" :weight bold))))

      (setq org-enforce-todo-dependencies          t
            org-enforce-todo-checkbox-dependencies t
            org-use-fast-todo-selection t)

      (setq org-capture-templates
            `(
              ("t"
               "TODO"
               entry
               (file+headline ,(f-join org-directory
                                       "gtd"
                                       "captures.org")
                              "Tasks")
               "* TODO %^{Todo} %(org-set-tags) \n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
               :kill-buffer t
               :immediate-finish t)
              ("j"
               "TODO"
               entry
               (file+headline ,(f-join org-directory
                                       "gtd"
                                       "js.org")
                              "JS Tasks")
               "* TODO %^{Todo} %(org-set-tags) \n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
               :kill-buffer t
               :immediate-finish t)
              )
            ))

    ("settings smartparens")
    (lambda ()
      ;; todo: make it smarter
      ;; (sp-local-pair 'org-mode "*" "*"
      ;;                :unless '(sp-point-at-bol-p))
      ;; (sp-local-pair 'org-mode "/" "/")
      ;; (sp-local-pair 'org-mode "_" "_")
      ;; (sp-local-pair 'org-mode "=" "=")
      ;; (sp-local-pair 'org-mode "~" "~")
      ;; (sp-local-pair 'org-mode "+" "+")
      (sp-local-pair 'org-mode "\\[" "\\]")
      )

    ("keymap")
    (lambda ()
      (func/keymap/save org-mode-map)
      (func/keymap/create org-mode-map
                          ;; Cycling
                          "TAB"       #'org-cycle
                          "<C-tab>"   #'org-global-cycle
                          "RET"       #'org-open-at-point

                          ;;
                          "C-t e"     #'yas-expand

                          ;; Toggles
                          "C-c C-t i" #'org-toggle-inline-images
                          "C-c C-t l" #'org-toggle-link-display

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
                          "C-c h a"   #'org-archive-subtree
                          "C-c h n"   #'org-narrow-to-subtree

                          ;; Items
                          "C-c C-z i" #'serika-f/org/insert-item

                          ;; Checkbox
                          "C-c C-z v" #'serika-f/org/insert-checkbox
                          "C-c v t"   #'serika-f/org/toggle-checkbox

                          ;; Tables
                          ;; table.el
                          "C-c C-z |" #'org-table-create-with-table.el
                          "C-c t e"   #'org-edit-special

                          ;; org-table
                          "C-c C-z \\" #'org-table-create
                          "C-c t a"    #'org-table-align
                          "C-c t r"    #'serika-f/org/recalculate-row
                          "C-c t R"    #'serika-f/org/recalculate-table
                          "C-c t d c"  #'org-table-delete-column
                          "C-c t d r"  #'org-table-delete-row
                          "C-c t n r"  #'org-table-insert-row
                          "C-c t n c"  #'org-table-insert-column
                          "C-c t n -"  #'org-table-insert-hline
                          "C-c t n _"  #'org-table-hline-and-move
                          "C-c t m n"  #'org-table-move-column-left
                          "C-c t m e"  #'org-table-move-row-down
                          "C-c t m i"  #'org-table-move-row-up
                          "C-c t m o"  #'org-table-move-column-right

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
                          "C-c C-c c" #'org-babel-execute-src-block
                          "C-c C-c e" #'serika-f/org/edit-src

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
                          "C-c n s"   #'serika-f/org/store-note))
    ;; todo: fix it
    ;; ("settings aggressive-indent")
    ;; (lambda ()
    ;;   (add-to-list 'aggressive-indent-dont-indent-if
    ;;                '(and (derived-mode-p 'org-mode)
    ;;                      (let ((item (org-element-at-point)))
    ;;                        (and item
    ;;                             (eq 'src-block
    ;;                                 (car item)))))))

    ("hook")
    (lambda ()
      (add-hook 'org-mode-hook #'serika-f/org//setup-buffer))

    ("global-keymap")
    (lambda ()
      (func/keymap/define-global "C-x <C-o> C-a" #'org-agenda)))

  ;; todo: Doesn't work
  ;; (serika-c/eg/add :name    'org-evil-insert
  ;;                  :parents '("keymap org"
  ;;                             "keymap evil insert")
  ;;                  :func    '(evil-define-key 'insert (symbol-value 'org-mode-map)
  ;;                              "<return>" #'newline-and-indent))
  )
