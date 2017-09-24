;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions
;; Char-state
(defun serika-f/evil/yank-char (&optional count)
  "Yank char."
  (interactive "P")
  (evil-visual-char)
  (evil-forward-char (or (1- count) 0))
  (evil-yank (region-beginning) (region-end))
  (evil-exit-visual-state))

(defun serika-f/evil/mark-char (&optional count)
  "Mark COUNT chars."
  (interactive "P")
  (evil-visual-char)
  (forward-char (or (1- count) 0)))

;; Word-state
(defun serika-f/evil/delete-word (&optional count)
  "Delete COUNT words."
  (interactive "P")
  (evil-visual-char)
  (evil-inner-word (or count 1))
  (evil-delete (region-beginning) (region-end)))

(defun serika-f/evil/yank-word (&optional count)
  "Yank word"
  (interactive "P")
  (save-excursion
    (evil-visual-char)
    (evil-inner-word (or count 1))
    (evil-yank (region-beginning) (region-end))
    (evil-exit-visual-state)))

(defun serika-f/evil/change-word (&optional count)
  "Yank word"
  (interactive "P")
  (evil-visual-char)
  (evil-inner-word (or count 1))
  (evil-change (region-beginning) (region-end)))

(defun serika-f/evil/mark-word (&optional count)
  "Mark COUNT words."
  (interactive "P")
  (evil-visual-char)
  (evil-inner-word (or count 1)))

(defun serika-f/evil/backward-word-at-previous-line (&optional count)
  "Move to the COUNT previous visual lines, and move to the previous word."
  (interactive "P")
  (evil-previous-line (or count 1))
  (evil-backward-word-begin))

(defun serika-f/evil/forward-word-at-next-line (&optional count)
  "Move to the COUNT previous visual lines, and move to the previous word."
  (interactive "P")
  (evil-next-line (or count 1))
  (evil-next-word-begin))

;; Line-state
(defun serika-f/evil/clear-line (&optional count)
  (interactive "P")
  (evil-visual-char)
  (end-of-line (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-exit-visual-state))

(defun serika-f/evil/delete-line (&optional count)
  (interactive "P")
  (beginning-of-line)
  (forward-char -1)
  (evil-visual-char)
  (forward-char +1)
  (end-of-line (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-exit-visual-state))

(defun serika-f/evil/yank-line (&optional count)
  (interactive "P")
  (save-excursion
    (evil-visual-char)
    (end-of-line (or count 1))
    (evil-yank (region-beginning) (region-end)))
  (evil-exit-visual-state))

(defun serika-f/evil/yank-whole-line (&optional count)
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (evil-visual-char)
    (end-of-line (or count 1))
    (evil-yank (region-beginning) (region-end)))
  (evil-exit-visual-state))

(defun serika-f/evil/change-line (&optional count)
  (interactive "P")
  (evil-visual-char)
  (end-of-line (or count 1))
  (evil-change (region-beginning) (region-end)))

(defun serika-f/evil/change-whole-line (&optional count)
  (interactive "P")
  (evil-beginning-of-line)
  (evil-visual-line)
  (end-of-line (or count 1))
  (evil-change (region-beginning) (region-end)))

(defun serika-f/evil/mark-line (&optional count)
  "Mark COUNT lines."
  (interactive "P")
  (evil-visual-line)
  (evil-next-line (or (and count (1- count)) 0)))

(defun serika-f/evil/insert-blank-line-after (&optional count)
  "Insert COUNT blank lines after current line."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (open-line (or count 1))))

(defun serika-f/evil/insert-blank-line-before (&optional count)
  "Insert COUNT blank lines before current line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (insert "\n")
    (forward-line -1)
    (open-line (or (and count (1- count)) 0)))
  (forward-line (or count 1)))

;; Paragraph-state
(defun serika-f/evil/kill-a-paragraph (&optional count)
  "Kill paragragh."
  (interactive "P")
  (evil-visual-char)
  (evil-a-paragraph (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-exit-visual-state))

(defun serika-f/evil/kill-inner-paragraph (&optional count)
  "Kill paragragh."
  (interactive "P")
  (evil-visual-char)
  (evil-inner-paragraph (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-exit-visual-state))

(defun serika-f/evil/yank-inner-paragraph (&optional count)
  "Kill paragragh."
  (interactive "P")
  (save-excursion
    (evil-visual-char)
    (evil-inner-paragraph (or count 1))
    (evil-yank (region-beginning) (region-end))
    (evil-exit-visual-state)))

(defun serika-f/evil/yank-a-paragraph (&optional count)
  "Kill paragragh."
  (interactive "P")
  (save-excursion
    (evil-visual-char)
    (evil-a-paragraph (or count 1))
    (evil-yank (region-beginning) (region-end))
    (evil-exit-visual-state)))

(defun serika-f/evil/change-inner-paragraph (&optional count)
  "Change COUNT inner paragragh."
  (interactive "P")
  (evil-visual-char)
  (evil-inner-paragraph (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-insert 1))

(defun serika-f/evil/change-a-paragraph (&optional count)
  "Change COUNT paragragh."
  (interactive "P")
  (evil-visual-char)
  (evil-a-paragraph (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-insert 1))

(defun serika-f/evil/mark-paragragh (&optional count)
  "Mark paragraph"
  (interactive "P")
  (evil-visual-char)
  (evil-inner-paragraph (or count 1)))

;; Visual state
(defun serika-f/evil/visual-n (&optional count)
  "Visual n movement."
  (interactive "P")
  (cond
   ((eq evil-previous-state
        'char)
    (backward-char (or count 1)))
   ((eq evil-previous-state
        'word)
    (evil-backward-word-begin (or count 1)))
   ((eq evil-previous-state
        'sentence)
    (ignore))
   ((eq evil-previous-state
        'line)
    (ignore))
   ((eq evil-previous-state
        'paragragh)
    (ignore))))

(defun serika-f/evil/visual-e (&optional count)
  "Visual n movement."
  (interactive "P")
  (cond
   ((eq evil-previous-state
        'char)
    (evil-next-line (or count 1)))
   ((eq evil-previous-state
        'word)
    (serika-f/evil/forward-word-at-next-line (or count 1)))
   ((eq evil-previous-state
        'sentence)
    (ignore))
   ((eq evil-previous-state
        'line)
    (evil-next-line (or count 1)))
   ((eq evil-previous-state
        'paragragh)
    (evil-forward-paragraph (or count 1)))))

(defun serika-f/evil/visual-i (&optional count)
  "Visual n movement."
  (interactive "P")
  (cond
   ((eq evil-previous-state
        'char)
    (evil-previous-line (or count 1)))
   ((eq evil-previous-state
        'word)
    (serika-f/evil/backward-word-at-previous-line (or count 1)))
   ((eq evil-previous-state
        'sentence)
    (ignore))
   ((eq evil-previous-state
        'line)
    (evil-previous-line (or count 1)))
   ((eq evil-previous-state
        'paragragh)
    (evil-backward-paragraph (or count 1)))))

(defun serika-f/evil/visual-o (&optional count)
  "Visual n movement."
  (interactive "P")
  (cond
   ((eq evil-previous-state
        'char)
    (forward-char (or count 1)))
   ((eq evil-previous-state
        'word)
    (evil-forward-word-begin (or count 1)))
   ((eq evil-previous-state
        'sentence)
    (ignore))
   ((eq evil-previous-state
        'line)
    (ignore))
   ((eq evil-previous-state
        'paragragh)
    (ignore))))

;; Functions
(defun evil-mode-p ()
  "Return t if evil mode is active"
  (or evil-mode evil-local-mode))

(defun serika-f/evil/change-to-previous-state ()
  "Change to previous state. If previous state is visual, change again."
  (interactive)
  (evil-change-to-previous-state)
  (when (eq evil-state
            'visual)
    (evil-change-to-previous-state)))

(defun serika-f/evil/forward-char ()
  "Forward char if next char is not a \n char."
  (interactive)
  (when (not (eolp))
    (forward-char)))

(defun serika-f/evil/backward-char ()
  "Forward char if next char is not a \n char."
  (interactive)
  (when (not (bolp))
    (backward-char)))

(defun serika-f/evil/replace-buffer ()
  "Replace all text in current buffer with text from clipboard."
  (interactive)
  (erase-buffer)
  (x-clipboard-yank))

(defun serika-f/evil/change-buffer ()
  "Replace all text in current buffer with text from clipboard."
  (interactive)
  (erase-buffer)
  (when (evil-mode-p)
    (evil-insert-state)))

(cl-defun serika-f/evil/activate (&key ((:evil-shift-width --evil-shift-width) 4           --evil-shift-width-p)
                                       ((:evil-state       --evil-state)       'metanormal --evil-state-p))
  "Activate `evil' in current buffer.
Supported keys:
  - evil-state:
    Set initial evil state.
    If wasn't provided, activate `evil-normal-state'.
  - evil-shift-width:
    If provided, change `evil-default-state' to its' value."
  (evil-local-mode +1)
  (if --evil-state-p
      (let ((--state-name (concat "evil-"
                                  (symbol-name --evil-state)
                                  "-state")))
        (if (evil-state-p --evil-state)
            (funcall (symbol-function (intern --state-name)))
          (error "Strange state was proposed for evil")))
    (evil-normal-state))
  (when --evil-shift-width-p
    (setq evil-shift-width --evil-shift-width)))

;; todo: remove this
(defun serika-f/evil/toggle ()
  "Toggle evil mode."
  (interactive)
  (cond
   (evil-mode
    (evil-mode -1))
   (evil-local-mode
    (evil-local-mode -1))
   (t
    (evil-local-mode +1)
    (evil-normal-state))))

;; Init
(defun init ()
  "Configure `evil'."
  ;; Configuration
  (serika-c/eg/add-install :type 'git
                           :name 'evil
                           :src "https://github.com/mikae/evil")

  (serika-c/eg/add :parents '("require"
                              "require ace-jump-mode"
                              "require ace-window"
                              "require evil-snipe")
                   :name 'evil
                   :func (lambda ()
                           (require 'evil)))

  (serika-c/eg/add :parents '("settings"
                              "settings ace-jump-mode"
                              "settings ace-window"
                              "settings evil-snipe")
                   :name 'evil
                   :func (lambda ()
                           (evil-define-state metanormal
                             "Metanormal state."
                             :tag "<MN>"
                             :suppress-keymap t)

                           (evil-define-state char
                             "Char mode."
                             :tag "<c>"
                             :suppress-keymap t)

                           (evil-define-state word
                             "Word mode."
                             :tag "<w>"
                             :suppress-keymap t)

                           (evil-define-state line
                             "Line mode."
                             :tag "<l>"
                             :suppress-keymap t)

                           (evil-define-state paragraph
                             "Paragragh mode."
                             :tag "<p>"
                             :suppress-keymap t)

                           (evil-define-state buffer
                             "Buffer mode."
                             :tag "<b>"
                             :suppress-keymap t)

                           (evil-define-state window
                             "Window mode."
                             :tag "<Wi>"
                             :suppress-keymap t)))

  (serika-c/eg/add-many-by-name
   'evil
   ("global-keymap")
   (lambda ()
     (func/keymap/define-global "C-x t e" #'serika-f/evil/toggle
                                "M-s" 'evil-ex

                                ;; arst
                                "A-a" 'evil-backward-word-begin
                                "A-A" 'evil-backward-WORD-begin
                                "A-r" 'evil-forward-word-begin
                                "A-R" 'evil-forward-WORD-begin
                                "A-s" 'evil-forward-word-end
                                "A-S" 'evil-forward-WORD-end
                                "A-t" 'evil-goto-first-line
                                "A-T" 'evil-goto-line

                                ;; qwfpg
                                "A-q" 'evil-find-char
                                "A-Q" 'evil-find-char-backward
                                "A-w" 'evil-find-char-to
                                "A-W" 'evil-find-char-to-backward
                                "A-f" 'evil-forward-paragraph
                                "A-F" 'evil-backward-paragraph
                                "A-p" 'evil-scroll-page-down
                                "A-P" 'evil-scroll-page-up

                                ;; zxcvb
                                "A-z"  'evil-search-next
                                "A-Z"  'evil-search-previous
                                "A-X"  'evil-jump-forward
                                "A-x"  'evil-jump-backward
                                "A-c"  'ace-jump-word-mode
                                "A-C"  'ace-jump-char-mode
                                "A-v"  'ace-jump-line-mode

                                ;; <Tab>123
                                "<A-tab>" #'evil-jump-item
                                "A-1"     #'evil-search-forward
                                "A-!"     #'evil-search-backward
                                "A-2"     #'vr/isearch-forward
                                "A-@"     #'vr/isearch-backward
                                "A-3"     #'vr/replace
                                "A-#"     #'vr/query-replace

                                ;; neio'
                                "A-n"  'evil-backward-char
                                "A-e"  'evil-next-line
                                "A-i"  'evil-previous-line
                                "A-o"  'evil-forward-char

                                "A-N"  'evil-beginning-of-line
                                "A-E"  'evil-window-bottom
                                "A-I"  'evil-window-top
                                "A-O"  'end-of-line
                                "A-\"" 'evil-window-middle

                                "A-." #'undo
                                "A-," #'redo)))

  (serika-c/eg/add-many-by-parents
   ("keymap evil")
   'metanormal
   (lambda ()
     (func/keymap/define evil-metanormal-state-map
                         "q" #'evil-normal-state
                         "Q" #'evil-emacs-state))
   'emacs
   (lambda ()
     (func/keymap/define evil-emacs-state-map
                         "SPC" #'evil-metanormal-state))

   'normal
   (lambda ()
     (func/keymap/define evil-normal-state-map
                         ;; todo: find out why is it needed
                         "C-t" nil

                         ;; qwfpg
                         "q" #'evil-char-state
                         "Q" #'evil-word-state
                         "w" #'evil-line-state
                         "W" #'evil-paragraph-state
                         "f" #'evil-buffer-state
                         "F" #'evil-window-state

                         ;; arstd
                         ;; "d" #'evil-visual-block

                         ;; zxcvb
                         "z" #'evil-delete
                         "x" #'evil-yank
                         "c" #'evil-change
                         "v" #'evil-paste-after
                         "V" #'evil-paste-before

                         ;; km,./
                         "," #'evil-repeat
                         "<" #'evil-use-register
                         "." #'undo
                         ">" #'redo

                         "C-SPC" #'evil-metanormal-state)

     (func/keymap/bind-digits evil-normal-state-map
                              #'digit-argument))

   'char
   (lambda ()
     (func/keymap/define evil-char-state-map
                         ;; qwfpg
                         "q" #'evil-find-char
                         "Q" #'evil-find-char-backward
                         "w" #'evil-find-char-to
                         "W" #'evil-find-char-to-backward
                         "f" #'evil-ace-jump-char-mode
                         "F" #'evil-ace-jump-char-to-mode
                         "C-SPC" #'ignore

                         ;; arstd
                         "a" #'evil-insert
                         "A" #'evil-insert-line
                         "r" #'evil-append
                         "R" #'evil-append-line
                         "s" #'evil-replace
                         "S" #'evil-replace-state
                         "t" #'evil-invert-char
                         "d" #'serika-f/evil/mark-char

                         ;; neio
                         "n" #'evil-backward-char
                         "e" #'evil-next-line
                         "i" #'evil-previous-line
                         "o" #'evil-forward-char

                         ;; zxcvb
                         "z" #'evil-delete-char
                         "x" #'serika-f/evil/yank-char
                         "v" #'evil-paste-after
                         "V" #'evil-paste-before

                         ;; km,./
                         "," #'evil-repeat
                         "<" #'evil-use-register
                         "." #'undo
                         ">" #'redo

                         "SPC" #'evil-normal-state))

   'word
   (lambda ()
     (func/keymap/define evil-word-state-map
                         ;; qwfpg
                         "q" #'evil-forward-word-end
                         "C-SPC" #'ignore

                         ;;arstd
                         "d" #'serika-f/evil/mark-word

                         ;; neio
                         "n" #'evil-backward-word-begin
                         "e" #'serika-f/evil/forward-word-at-next-line
                         "i" #'serika-f/evil/backward-word-at-previous-line
                         "o" #'evil-forward-word-begin

                         ;; zxcvb
                         "z" #'serika-f/evil/delete-word
                         "x" #'serika-f/evil/yank-word
                         "c" #'serika-f/evil/change-word
                         "v" #'evil-paste-after
                         "V" #'evil-paste-before

                         ;; km,./
                         "," #'evil-repeat
                         "<" #'evil-use-register
                         "." #'undo
                         ">" #'redo

                         "SPC" 'evil-normal-state))

   'line
   (lambda ()
     (func/keymap/define evil-line-state-map
                         ;; qwfpg

                         ;; arstd
                         "a" #'evil-join
                         "A" #'evil-join-whitespace
                         "r" #'evil-open-below
                         "R" #'evil-open-above
                         "s" #'serika-f/evil/insert-blank-line-after
                         "S" #'serika-f/evil/insert-blank-line-before
                         "d" #'serika-f/evil/mark-line

                         "A-a" #'ace-jump-line-mode

                         ;; neio
                         "n" #'evil-backward-char
                         "e" #'evil-next-line
                         "i" #'evil-previous-line
                         "o" #'evil-forward-char

                         "N" #'evil-beginning-of-line
                         "E" #'evil-window-bottom
                         "I" #'evil-window-top
                         "O" #'end-of-line

                         ;; zxcvb
                         "z" #'serika-f/evil/clear-line
                         "Z" #'serika-f/evil/delete-line
                         "x" #'serika-f/evil/yank-line
                         "X" #'serika-f/evil/yank-whole-line
                         "c" #'serika-f/evil/change-line
                         "C" #'serika-f/evil/change-whole-line
                         "v" #'evil-paste-after
                         "V" #'evil-paste-before

                         ;; km,./
                         "," #'evil-repeat
                         "<" #'evil-use-register
                         "." #'undo
                         ">" #'redo
                         "C-SPC" #'ignore

                         "SPC" #'evil-normal-state))

   'paragragh
   (lambda ()
     (func/keymap/define evil-paragraph-state-map
                         ;; qwfpg
                         "C-SPC" #'ignore

                         ;; arstd
                         "d" #'serika-f/evil/mark-paragragh

                         ;; zxcvb
                         "z" #'serika-f/evil/kill-inner-paragraph
                         "Z" #'serika-f/evil/kill-a-paragraph
                         "x" #'serika-f/evil/yank-inner-paragraph
                         "X" #'serika-f/evil/yank-a-paragraph
                         "c" #'serika-f/evil/change-inner-paragraph
                         "C" #'serika-f/evil/change-a-paragraph
                         "v" #'evil-paste-after
                         "V" #'evil-paste-before

                         ;; neio
                         "n" #'evil-backward-char
                         "e" #'evil-forward-paragraph
                         "i" #'evil-backward-paragraph
                         "o" #'evil-forward-char

                         ;; km,./
                         "," #'evil-repeat
                         "<" #'evil-use-register
                         "." #'undo
                         ">" #'redo

                         "SPC" #'evil-normal-state))

   'buffer
   (lambda ()
     (func/keymap/define evil-buffer-state-map
                         ;; qwfpg
                         "C-SPC" #'ignore

                         ;; arst
                         "z" #'kill-buffer

                         ;; neio
                         "n" #'previous-buffer
                         "e" #'previous-buffer
                         "i" #'next-buffer
                         "o" #'next-buffer

                         "SPC" #'evil-normal-state))
   'window
   (lambda ()
     (func/keymap/define evil-window-state-map
                         ;; qwfpg
                         "q" #'ace-window
                         "C-SPC" #'ignore

                         ;; arst
                         "a" #'evil-window-vsplit
                         "A" #'evil-window-split

                         ;; neio
                         "n" #'evil-window-left
                         "e" #'evil-window-down
                         "i" #'evil-window-up
                         "o" #'evil-window-right

                         "N" #'buf-move-left
                         "E" #'buf-move-down
                         "I" #'buf-move-up
                         "O" #'buf-move-right

                         "C-n"   #'shrink-window-horizontally
                         "C-e"   #'enlarge-window
                         "C-i"   #'shrink-window
                         "<C-o>" #'enlarge-window-horizontally

                         ;; zxcvb
                         "z" #'delete-window

                         "SPC" #'evil-normal-state))

   'motion
   (lambda ()
     (func/keymap/define evil-motion-state-map
                         "<deletechar>" 'ignore
                         "DEL"          'ignore))

   'insert
   (lambda ()
     (func/keymap/define evil-insert-state-map
                         "A-n" #'serika-f/evil/backward-char
                         "A-o" #'serika-f/evil/forward-char

                         ;; Ret with proper indentation
                         "RET" #'newline

                         ;; Exit insert state
                         "C-SPC" 'serika-f/evil/change-to-previous-state))

   'replace
   (lambda ()
     (func/keymap/define evil-replace-state-map
                         "SPC" 'evil-change-to-previous-state))

   'operator-state
   (lambda ()
     (func/keymap/define evil-operator-state-map
                         "SPC" #'evil-change-to-previous-state

                         "a"     evil-outer-text-objects-map
                         "r"     evil-inner-text-objects-map))

   'text-objects
   (lambda ()
     (func/keymap/define evil-outer-text-objects-map
                         "a" #'evil-a-word
                         "A" #'evil-a-WORD
                         "r" #'evil-a-paren
                         "R" #'evil-a-curly
                         "s" #'evil-a-bracket
                         "S" #'evil-an-angle
                         "t" #'evil-a-single-quote
                         "T" #'evil-a-double-quote

                         "z" #'evil-a-back-quote
                         "Z" #'evil-a-tag
                         "x" #'evil-a-sentence
                         "X" #'evil-a-paragraph)

     (func/keymap/define evil-inner-text-objects-map
                         "a" #'evil-inner-word
                         "A" #'evil-inner-WORD
                         "r" #'evil-inner-paren
                         "R" #'evil-inner-curly
                         "s" #'evil-inner-bracket
                         "S" #'evil-inner-angle
                         "t" #'evil-inner-single-quote
                         "T" #'evil-inner-double-quote

                         "z" #'evil-inner-back-quote
                         "Z" #'evil-inner-tag
                         "x" #'evil-inner-sentence
                         "X" #'evil-inner-paragraph))

   'ex-keymap
   (lambda ()
     (func/keymap/define evil-ex-completion-map
                         "RET" #'exit-minibuffer))
   'visual
   (lambda ()
     (func/keymap/define evil-visual-state-map
                         "A-1" #'evil-visualstar/begin-search-forward
                         "A-!" #'evil-visualstar/begin-search-forward

                         "TAB" #'er/expand-region

                         "a"   evil-outer-text-objects-map
                         "r"   evil-inner-text-objects-map

                         "A"   #'evil-append
                         "R"   #'evil-insert

                         "n"   #'serika-f/evil/visual-n
                         "e"   #'serika-f/evil/visual-e
                         "i"   #'serika-f/evil/visual-i
                         "o"   #'serika-f/evil/visual-o

                         "SPC"   #'evil-exit-visual-state
                         "C-SPC" #'ignore))))
