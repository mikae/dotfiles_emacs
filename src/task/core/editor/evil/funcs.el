;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/evil/replace-buffer ()
  "Replace all text in current buffer with text from clipboard."
  (interactive)
  (erase-buffer)
  (x-clipboard-yank))

;; Global
(defun serika-g/evil//require ()
  "Require modules for `evil'."
  (require 'func-package)
  (require 'func-keymap)

  (require 'evil)
  )

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
  ;; Standard vim bindings
  (define-key evil-normal-state-map (kbd "a")   'evil-append)
  (define-key evil-normal-state-map (kbd "A")   'evil-append-line)
  (define-key evil-normal-state-map (kbd "c")   'evil-change)
  (define-key evil-normal-state-map (kbd "C")   'evil-change-line)
  (define-key evil-normal-state-map (kbd "d")   'evil-delete)
  (define-key evil-normal-state-map (kbd "D")   'evil-delete-line)
  (define-key evil-normal-state-map (kbd "i")   'evil-insert)
  (define-key evil-normal-state-map (kbd "I")   'evil-insert-line)
  (define-key evil-normal-state-map (kbd "m")   'evil-set-marker)
  (define-key evil-normal-state-map (kbd "o")   'evil-open-below)
  (define-key evil-normal-state-map (kbd "O")   'evil-open-above)
  (define-key evil-normal-state-map (kbd "p")   'evil-paste-after)
  (define-key evil-normal-state-map (kbd "y")   'evil-yank)
  (define-key evil-normal-state-map (kbd "Y")   'evil-yank-line)
  (define-key evil-normal-state-map (kbd "P")   'evil-paste-before)
  (define-key evil-normal-state-map (kbd "r")   'evil-replace)
  (define-key evil-normal-state-map (kbd "R")   'evil-replace-state)
  (define-key evil-normal-state-map (kbd "s")   'evil-substitute)
  (define-key evil-normal-state-map (kbd "S")   'evil-change-whole-line)
  (define-key evil-normal-state-map (kbd "x")   'evil-delete-char)
  (define-key evil-normal-state-map (kbd "y")   'evil-yank)
  (define-key evil-normal-state-map (kbd ".")   'evil-repeat)
  (define-key evil-normal-state-map (kbd "~")   'evil-invert-char)
  (define-key evil-normal-state-map (kbd "<")   'evil-shift-left)
  (define-key evil-normal-state-map (kbd ">")   'evil-shift-right)
  (define-key evil-normal-state-map (kbd "\"")  'evil-use-register)
  (define-key evil-normal-state-map (kbd "'")   'evil-goto-mark-line)

  ;; Hyper-actions
  (define-key evil-normal-state-map (kbd "H-d") 'erase-buffer)
  (define-key evil-normal-state-map (kbd "H-s") 'serika-f/evil/replace-buffer)

  ;; Undo-redo
  (define-key evil-normal-state-map (kbd "u")   'undo)
  (define-key evil-normal-state-map (kbd "U")   'redo)

  ;; Visual selection
  (define-key evil-normal-state-map (kbd "v" )  'evil-visual-char)
  (define-key evil-normal-state-map (kbd "V" )  'evil-visual-line)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-block)

  ;; Macro
  (define-key evil-normal-state-map (kbd "q")   'kmacro-start-macro-or-insert-counter)
  (define-key evil-normal-state-map (kbd "Q")   'kmacro-end-or-call-macro)

  ;; Go to emacs state
  (define-key evil-normal-state-map (kbd "<C-m> e") 'evil-emacs-state))

(defun serika-g/evil//motion-keymap ()
  "Configure `evil-motion-state-map'."
  (define-key evil-motion-state-map (kbd "A-'") 'evil-goto-mark))

(defun serika-g/evil//insert-keymap ()
  "Configure `evil-insert-state-map'."
  ;; Redefine motions in `insert' state, because other state
  ;; expects `evil' motions
  (define-key evil-insert-state-map (kbd "A-h")   'evil-backward-char)
  (define-key evil-insert-state-map (kbd "A-l")   'evil-forward-char)

  ;; Ret with proper indentation
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

  ;; Exit insert state
  (define-key evil-insert-state-map (kbd "C-, C-j") 'evil-normal-state))

(defun serika-g/evil//visual-keymap ()
  "Configure `evil-visual-state-map'."
  (define-key evil-visual-state-map "a" evil-outer-text-objects-map)
  (define-key evil-visual-state-map "i" evil-inner-text-objects-map)

  (define-key evil-visual-state-map "A" 'evil-append)
  (define-key evil-visual-state-map "I" 'evil-insert)
  (define-key evil-visual-state-map "u" 'evil-downcase)
  (define-key evil-visual-state-map "U" 'evil-upcase)

  (define-key evil-visual-state-map (kbd "C-, C-j") 'evil-exit-visual-state))

(defun serika-g/evil//replace-keymap ()
  "Configure `evil-replace-state-map'."
  (define-key evil-replace-state-map (kbd "C-, C-j") 'evil-normal-state))

(defun serika-g/evil//emacs-keymap ()
  "Configure `evil-emacs-state-map'."
  (define-key evil-emacs-state-map (kbd "<C-m> v") 'evil-normal-state))

(defun serika-g/evil//operator-state-map ()
  "Configure `evil-operator-state-map'."
  (define-key evil-emacs-state-map (kbd "<C-m> v") 'evil-normal-state)
  (define-key evil-operator-state-map "a" evil-outer-text-objects-map)
  (define-key evil-operator-state-map "i" evil-inner-text-objects-map))

(defun serika-g/evil//text-objects-map ()
  "Configure `evil-operator-state-map'."
  (define-key evil-outer-text-objects-map "w" 'evil-a-word)
  (define-key evil-outer-text-objects-map "W" 'evil-a-WORD)
  (define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
  (define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)
  (define-key evil-outer-text-objects-map "b" 'evil-a-paren)
  (define-key evil-outer-text-objects-map "(" 'evil-a-paren)
  (define-key evil-outer-text-objects-map ")" 'evil-a-paren)
  (define-key evil-outer-text-objects-map "[" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "]" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "B" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "{" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "}" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "<" 'evil-an-angle)
  (define-key evil-outer-text-objects-map ">" 'evil-an-angle)
  (define-key evil-outer-text-objects-map "'" 'evil-a-single-quote)
  (define-key evil-outer-text-objects-map "\"" 'evil-a-double-quote)
  (define-key evil-outer-text-objects-map "`" 'evil-a-back-quote)
  (define-key evil-outer-text-objects-map "t" 'evil-a-tag)
  (define-key evil-outer-text-objects-map "o" 'evil-a-symbol)
  (define-key evil-inner-text-objects-map "w" 'evil-inner-word)
  (define-key evil-inner-text-objects-map "W" 'evil-inner-WORD)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
  (define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
  (define-key evil-inner-text-objects-map "b" 'evil-inner-paren)
  (define-key evil-inner-text-objects-map "(" 'evil-inner-paren)
  (define-key evil-inner-text-objects-map ")" 'evil-inner-paren)
  (define-key evil-inner-text-objects-map "[" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "]" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "B" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "{" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "}" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "<" 'evil-inner-angle)
  (define-key evil-inner-text-objects-map ">" 'evil-inner-angle)
  (define-key evil-inner-text-objects-map "'" 'evil-inner-single-quote)
  (define-key evil-inner-text-objects-map "\"" 'evil-inner-double-quote)
  (define-key evil-inner-text-objects-map "`" 'evil-inner-back-quote)
  (define-key evil-inner-text-objects-map "t" 'evil-inner-tag)
  (define-key evil-inner-text-objects-map "o" 'evil-inner-symbol))

(defun serika-g/evil//global-keymap ()
  "Add movements into Emacs global keymap."
  ;; `evil-ex'
  (global-set-key (kbd "M-;") 'evil-ex)

  ;; Window open functions
  (global-set-key (kbd "C-, w k") 'evil-window-up)
  (global-set-key (kbd "C-, w l") 'evil-window-right)
  (global-set-key (kbd "C-, w j") 'evil-window-down)
  (global-set-key (kbd "C-, w h") 'evil-window-left)
  (global-set-key (kbd "C-, w w") 'evil-window-vsplit)
  (global-set-key (kbd "C-, w W") 'evil-window-split)
  (global-set-key (kbd "C-, w q") 'evil-window-delete)

  ;; Word movements
  (global-set-key (kbd "A-w")     'evil-forward-word-begin)
  (global-set-key (kbd "A-W")     'evil-forward-WORD-begin)
  (global-set-key (kbd "A-b")     'evil-backward-word-begin)
  (global-set-key (kbd "A-B")     'evil-backward-WORD-begin)
  (global-set-key (kbd "A-e")     'evil-forward-word-end)
  (global-set-key (kbd "A-E")     'evil-forward-WORD-end)

  ;; Search
  (global-set-key (kbd "A-*")     'evil-search-word-forward)
  (global-set-key (kbd "A-/")     'evil-search-forward)
  (global-set-key (kbd "A-?")     'evil-search-backward)

  ;; Find
  (global-set-key (kbd "A-f")     'evil-find-char)
  (global-set-key (kbd "A-F")     'evil-find-char-backward)
  (global-set-key (kbd "A-t")     'evil-find-char-to)
  (global-set-key (kbd "A-T")     'evil-find-char-to-backward)
  (global-set-key (kbd "A-n")     'evil-search-next)
  (global-set-key (kbd "A-N")     'evil-search-previous)

  ;; Paragraphs
  (global-set-key (kbd "A-[")     'evil-backward-paragraph)
  (global-set-key (kbd "A-]")     'evil-forward-paragraph)

  ;; Jumps
  (global-set-key (kbd "<A-tab>") 'evil-jump-item)
  (global-set-key (kbd "A-o")     'evil-jump-backward)
  (global-set-key (kbd "A-O")     'evil-jump-forward)

  (global-set-key (kbd "A-H")     'evil-beginning-of-visual-line)
  (global-set-key (kbd "A-J")     'evil-window-bottom)
  (global-set-key (kbd "A-K")     'evil-window-top)
  (global-set-key (kbd "A-L")     'evil-end-of-visual-line)
  (global-set-key (kbd "A-M")     'evil-window-middle)

  ;; Goto line
  (global-set-key (kbd "A-g")     'evil-goto-first-line)
  (global-set-key (kbd "A-G")     'evil-goto-line)

  ;; `evil' backward and forward `char' works not as expected in insert state
  (global-set-key (kbd "A-h")     'backward-char)
  (global-set-key (kbd "A-j")     'evil-next-visual-line)
  (global-set-key (kbd "A-k")     'evil-previous-visual-line)
  (global-set-key (kbd "A-l")     'forward-char)

  ;; Scrollings
  ;; Hyper because no more empty modifiers, except <Meta>
  (global-set-key (kbd "A-H-f")   'evil-scroll-page-down)
  (global-set-key (kbd "A-H-b")   'evil-scroll-page-up)
  (global-set-key (kbd "A-H-d")   'evil-scroll-down)
  (global-set-key (kbd "A-H-u")   'evil-scroll-up)
  )

(defun init ()
  "Configure `evil'."
  ;; Configuration
  (serika-c/eg/add :parents '("require")
                   :name    'evil
                   :func    #'serika-g/evil//require)

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
                   :func    #'serika-g/evil//operator-state-map)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'text-objects
                   :func    #'serika-g/evil//text-objects-map)
  )
