;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun evil-mode-p ()
  "Return t if evil mode is active"
  (or evil-mode evil-local-mode))

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

(cl-defun serika-f/evil/activate (&key ((:evil-shift-width --evil-shift-width) 4       --evil-shift-width-p)
                                       ((:evil-state       --evil-state)       'normal --evil-state-p))
  "Activate `evil' in current buffer."
  (evil-local-mode +1)
  (if --evil-state-p
      (cond
       ((eq --evil-state
            'normal)
        (evil-normal-state))
       ((eq --evil-state
            'motion)
        (evil-motion-state))
       ((eq --evil-state
            'emacs)
        (evil-emacs-state))
       (t (error "Strange state war proposed to `evil'.")))
    (evil-normal-state))
  (when --evil-shift-width-p
    (setq evil-shift-width --evil-shift-width)))

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

;; Global
(defun serika-g/evil//settings ()
  "Configure `evil' variables."
  (setq evil-default-state 'emacs)

  (setq evil-insert-state-cursor    '((bar . 2) "yellow")
        evil-normal-state-cursor    '(box       "purple")
        evil-visual-state-cursor    '(box       "green")
        evil-motion-state-cursor    '((bar . 2) "orange")
        evil-replace-state-cursor   '(box       "red")
        evil-operator-state-cursor  '((bar . 2) "orange")
        evil-emacs-state-cursor     '(box       "cyan")))

(defun serika-g/evil//normal-keymap ()
  "Configure `evil-normal-state-map'."
  ;; qwfp
  (func/keymap/define evil-normal-state-map
                      "C-t" nil

                      "q"   #'evil-open-below
                      "Q"   #'evil-open-above
                      "w"   #'evil-replace
                      "W"   #'evil-replace-state
                      "f"   #'evil-delete-char
                      "F"   #'evil-invert-char
                      "p"   #'evil-join
                      "P"   #'evil-join-whitespace

                      "a"   #'evil-append
                      "A"   #'evil-append-line
                      "r"   #'evil-insert
                      "R"   #'evil-insert-line
                      "s"   #'evil-change
                      "S"   #'evil-change-line
                      "t"   #'evil-substitute
                      "T"   #'evil-change-whole-line

                      "z"   #'evil-delete
                      "Z"   #'evil-delete-line
                      "x"   #'evil-yank
                      "X"   #'evil-yank-line
                      "c"   #'evil-paste-after
                      "C"   #'evil-paste-before
                      "v"   #'evil-visual-char
                      "V"   #'evil-visual-line

                      ","   #'evil-repeat
                      "<"   #'evil-use-register
                      "."   #'undo
                      ">"   #'redo

                      "C-v" #'evil-visual-block

                      "H-z" #'erase-buffer
                      "H-x" #'serika-f/evil/replace-buffer
                      "H-c" #'serika-f/evil/change-buffer

                      "C-, C-n" 'evil-emacs-state))

(defun serika-g/evil//motion-keymap ()
  "Configure `evil-motion-state-map'."
  (func/keymap/define evil-motion-state-map
                          "<deletechar>" 'ignore
                          "DEL"          'ignore))

(defun serika-g/evil//insert-keymap ()
  "Configure `evil-insert-state-map'."
  (func/keymap/define evil-insert-state-map
                          "A-n" 'func/char/backward
                          "A-o" 'func/char/forward

                          ;; Ret with proper indentation
                          "RET" 'newline-and-indent

                          ;; Exit insert state
                          "C-, C-n" 'evil-normal-state))

(defun serika-g/evil//visual-keymap ()
  "Configure `evil-visual-state-map'."
  (func/keymap/define evil-visual-state-map
                      "a"   evil-outer-text-objects-map
                      "r"   evil-inner-text-objects-map

                      "A"   #'evil-append
                      "R"   #'evil-insert

                      "C-, C-n" 'evil-exit-visual-state))

(defun serika-g/evil//replace-keymap ()
  "Configure `evil-replace-state-map'."
  (func/keymap/define evil-replace-state-map
                      "C-, C-n" 'evil-normal-state))

(defun serika-g/evil//emacs-keymap ()
  "Configure `evil-emacs-state-map'."
  (func/keymap/define evil-emacs-state-map
                          "C-, C-n" 'evil-normal-state))

(defun serika-g/evil//operator-state-keymap ()
  "Configure `evil-operator-state-map'."
  (func/keymap/define evil-operator-state-map
                          "C-, C-n" 'evil-normal-state

                          "a"        evil-outer-text-objects-map
                          "r"        evil-inner-text-objects-map))

(defun serika-g/evil//text-objects-keymap ()
  "Configure text objects map."
  (func/keymap/define evil-outer-text-objects-map
                          "a" 'evil-a-word
                          "A" 'evil-a-WORD
                          "r" 'evil-a-paren
                          "R" 'evil-a-curly
                          "s" 'evil-a-bracket
                          "S" 'evil-an-angle
                          "t" 'evil-a-single-quote
                          "T" 'evil-a-double-quote

                          "z" 'evil-a-back-quote
                          "Z" 'evil-a-tag
                          "x" 'evil-a-sentence
                          "X" 'evil-a-paragraph)

  (func/keymap/define evil-inner-text-objects-map
                          "a" 'evil-inner-word
                          "A" 'evil-inner-WORD
                          "r" 'evil-inner-paren
                          "R" 'evil-inner-curly
                          "s" 'evil-inner-bracket
                          "S" 'evil-inner-angle
                          "t" 'evil-inner-single-quote
                          "T" 'evil-inner-double-quote

                          "z" 'evil-inner-back-quote
                          "Z" 'evil-inner-tag
                          "x" 'evil-inner-sentence
                          "X" 'evil-inner-paragraph))

(defun serika-g/evil//ex-keymap ()
  "Configure evil-ex."
  (func/keymap/define evil-ex-completion-map
                      "RET" 'exit-minibuffer))

(defun serika-g/evil//global-keymap ()
  "Add movements into Emacs global keymap."
  (func/keymap/define-global
   ;; `evil-ex'
   "M-s" 'evil-ex

   "C-x t e" #'serika-f/evil/toggle

   ;; Window operations
   "C-w n" 'evil-window-left
   "C-w e" 'evil-window-down
   "C-w i" 'evil-window-up
   "C-w o" 'evil-window-right
   "C-w q" 'evil-window-delete
   "C-w w" 'evil-window-vsplit
   "C-w W" 'evil-window-split

   ;; arst
   "A-a" 'evil-backward-word-begin
   "A-A" 'evil-backward-WORD-begin
   "A-r" 'evil-forward-word-begin
   "A-R" 'evil-forward-WORD-begin
   "A-s" 'evil-forward-word-end
   "A-S" 'evil-forward-WORD-end
   "A-t" 'evil-goto-first-line
   "A-T" 'evil-goto-line

   ;; qwfp
   "A-q" 'evil-find-char
   "A-Q" 'evil-find-char-backward
   "A-w" 'evil-find-char-to
   "A-W" 'evil-find-char-to-backward
   "A-f" 'evil-forward-paragraph
   "A-F" 'evil-backward-paragraph
   "A-p" 'evil-scroll-page-down
   "A-P" 'evil-scroll-page-up

   ;; zxcv
   "A-z"  'evil-search-next
   "A-Z"  'evil-search-previous
   "A-X"  'evil-jump-forward
   "A-x"  'evil-jump-backward

   ;; <Tab>!@
   "<A-tab>" #'evil-jump-item
   "A-1"     #'evil-search-forward
   "A-!"     #'evil-search-backward

   ;; neio'
   "A-n"  'evil-backward-char
   "A-N"  'evil-beginning-of-visual-line
   "A-e"  'evil-next-visual-line
   "A-E"  'evil-window-bottom
   "A-i"  'evil-previous-visual-line
   "A-I"  'evil-window-top
   "A-o"  'evil-forward-char
   "A-O"  'evil-end-of-visual-line
   "A-\"" 'evil-window-middle))

(defun init ()
  "Configure `evil'."
  ;; Configuration
  (serika-c/eg/add-install :type 'git
                           :name 'evil
                           :src "https://github.com/mikae/evil")

  (serika-c/eg/add-many-by-name 'evil
                        ("require")
                        (func/func/requirer evil))

  (serika-c/eg/add :parents '("settings")
                   :name    'evil
                   :func    #'serika-g/evil//settings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'evil
                   :func    #'serika-g/evil//global-keymap)

  ;; Keymaps
  (serika-c/eg/add :parents '("keymap evil")
                   :name    'normal
                   :func    #'serika-g/evil//normal-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'motion
                   :func    #'serika-g/evil//motion-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'insert
                   :func    #'serika-g/evil//insert-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'visual
                   :func    #'serika-g/evil//visual-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'replace
                   :func    #'serika-g/evil//replace-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'emacs
                   :func    #'serika-g/evil//emacs-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'operator-state
                   :func    #'serika-g/evil//operator-state-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'text-objects
                   :func    #'serika-g/evil//text-objects-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'ex-keymap
                   :func    #'serika-g/evil//ex-keymap))
