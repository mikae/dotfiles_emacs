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

;; Global
(defun serika-g/evil//require ()
  "Require modules for `evil'."
  (require 'func-package)
  (require 'func-keymap)

  (require 'evil))

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
  (define-key evil-normal-state-map (kbd "q")   'evil-open-below)
  (define-key evil-normal-state-map (kbd "Q")   'evil-open-above)
  (define-key evil-normal-state-map (kbd "w")   'evil-replace)
  (define-key evil-normal-state-map (kbd "W")   'evil-replace-state)
  (define-key evil-normal-state-map (kbd "f")   'evil-delete-char)
  (define-key evil-normal-state-map (kbd "F")   'evil-invert-char)
  (define-key evil-normal-state-map (kbd "p")   'evil-shift-left)
  (define-key evil-normal-state-map (kbd "P")   'evil-shift-right)

  ;; arst
  (define-key evil-normal-state-map (kbd "a")   'evil-append)
  (define-key evil-normal-state-map (kbd "A")   'evil-append-line)
  (define-key evil-normal-state-map (kbd "r")   'evil-insert)
  (define-key evil-normal-state-map (kbd "R")   'evil-insert-line)
  (define-key evil-normal-state-map (kbd "s")   'evil-change)
  (define-key evil-normal-state-map (kbd "S")   'evil-change-line)
  (define-key evil-normal-state-map (kbd "t")   'evil-substitute)
  (define-key evil-normal-state-map (kbd "T")   'evil-change-whole-line)

  ;; zxcv
  (define-key evil-normal-state-map (kbd "z")   'evil-delete)
  (define-key evil-normal-state-map (kbd "Z")   'evil-delete-line)
  (define-key evil-normal-state-map (kbd "x")   'evil-yank)
  (define-key evil-normal-state-map (kbd "X")   'evil-yank-line)
  (define-key evil-normal-state-map (kbd "c")   'evil-paste-after)
  (define-key evil-normal-state-map (kbd "C")   'evil-paste-before)
  (define-key evil-normal-state-map (kbd "v" )  'evil-visual-char)
  (define-key evil-normal-state-map (kbd "V" )  'evil-visual-line)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-block)

  ;; luy
  (define-key evil-normal-state-map (kbd "l")   'erase-buffer)
  (define-key evil-normal-state-map (kbd "u")   'serika-f/evil/replace-buffer)
  (define-key evil-normal-state-map (kbd "y")   'serika-f/evil/change-buffer)

  ;; m,./
  (define-key evil-normal-state-map (kbd "m")   'kmacro-start-macro-or-insert-counter)
  (define-key evil-normal-state-map (kbd "M")   'kmacro-end-or-call-macro)
  (define-key evil-normal-state-map (kbd ",")   'evil-repeat)
  (define-key evil-normal-state-map (kbd "<")   'evil-use-register)
  (define-key evil-normal-state-map (kbd ".")   'undo)
  (define-key evil-normal-state-map (kbd ">")   'redo)

  ;; Go to emacs state
  (define-key evil-normal-state-map (kbd "C-, C-n") 'evil-emacs-state))

(defun serika-g/evil//motion-keymap ()
  "Configure `evil-motion-state-map'."
  (define-key evil-motion-state-map (kbd "deletechar") 'ignore)
  (define-key evil-motion-state-map (kbd "DEL")        'ignore))

(defun serika-g/evil//insert-keymap ()
  "Configure `evil-insert-state-map'."
  (define-key evil-insert-state-map (kbd "A-n") 'backward-char)
  (define-key evil-insert-state-map (kbd "A-o") 'forward-char)

  ;; hungry deletion
  (define-key evil-insert-state-map (kbd "DEL") 'c-hungry-delete-backwards)
  (define-key evil-insert-state-map (kbd "<deletechar>") 'c-hungry-delete-forward)

  ;; Ret with proper indentation
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

  ;; Exit insert state
  (define-key evil-insert-state-map (kbd "C-, C-n") 'evil-normal-state))

(defun serika-g/evil//visual-keymap ()
  "Configure `evil-visual-state-map'."
  (define-key evil-visual-state-map (kbd "a") evil-outer-text-objects-map)
  (define-key evil-visual-state-map (kbd "r") evil-inner-text-objects-map)

  (define-key evil-visual-state-map (kbd "a") 'evil-append)
  (define-key evil-visual-state-map (kbd "r") 'evil-insert)

  (define-key evil-visual-state-map (kbd "C-, C-n") 'evil-exit-visual-state))

(defun serika-g/evil//replace-keymap ()
  "Configure `evil-replace-state-map'."
  (define-key evil-replace-state-map (kbd "C-, C-n") 'evil-normal-state))

(defun serika-g/evil//emacs-keymap ()
  "Configure `evil-emacs-state-map'."
  (define-key evil-emacs-state-map (kbd "C-, C-n") 'evil-normal-state))

(defun serika-g/evil//operator-state-keymap ()
  "Configure `evil-operator-state-map'."
  (define-key evil-operator-state-map (kbd "C-, C-n") 'evil-normal-state)

  (define-key evil-operator-state-map (kbd "a")        evil-outer-text-objects-map)
  (define-key evil-operator-state-map (kbd "r")        evil-inner-text-objects-map))

(defun serika-g/evil//text-objects-keymap ()
  "Configure `evil-operator-state-map'."
  (define-key evil-outer-text-objects-map "a" 'evil-a-word)
  (define-key evil-outer-text-objects-map "A" 'evil-a-WORD)
  (define-key evil-outer-text-objects-map "r" 'evil-a-paren)
  (define-key evil-outer-text-objects-map "R" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "s" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "S" 'evil-an-angle)
  (define-key evil-outer-text-objects-map "t" 'evil-a-single-quote)
  (define-key evil-outer-text-objects-map "T" 'evil-a-double-quote)

  (define-key evil-outer-text-objects-map "z" 'evil-a-back-quote)
  (define-key evil-outer-text-objects-map "Z" 'evil-a-tag)
  (define-key evil-outer-text-objects-map "x" 'evil-a-sentence)
  (define-key evil-outer-text-objects-map "X" 'evil-a-paragraph)

  (define-key evil-inner-text-objects-map "a" 'evil-inner-word)
  (define-key evil-inner-text-objects-map "A" 'evil-inner-WORD)
  (define-key evil-inner-text-objects-map "r" 'evil-inner-paren)
  (define-key evil-inner-text-objects-map "R" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "s" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "S" 'evil-inner-angle)
  (define-key evil-inner-text-objects-map "t" 'evil-inner-single-quote)
  (define-key evil-inner-text-objects-map "T" 'evil-inner-double-quote)

  (define-key evil-inner-text-objects-map "z" 'evil-inner-back-quote)
  (define-key evil-inner-text-objects-map "Z" 'evil-inner-tag)
  (define-key evil-inner-text-objects-map "x" 'evil-inner-sentence)
  (define-key evil-inner-text-objects-map "X" 'evil-inner-paragraph))

(defun serika-g/evil//ex-keymap ()
  "Configure evil-ex."
  (define-key evil-ex-completion-map [return]    'exit-minibuffer)
  (define-key evil-ex-completion-map (kbd "RET") 'exit-minibuffer))

(defun serika-g/evil//global-keymap ()
  "Add movements into Emacs global keymap."
  ;; `evil-ex'
  (global-set-key (kbd "M-s")     'evil-ex)

  ;; Window operations
  (global-set-key (kbd "C-, w n") 'evil-window-left)
  (global-set-key (kbd "C-, w e") 'evil-window-down)
  (global-set-key (kbd "C-, w i") 'evil-window-up)
  (global-set-key (kbd "C-, w o") 'evil-window-right)
  (global-set-key (kbd "C-, w q") 'evil-window-delete)
  (global-set-key (kbd "C-, w w") 'evil-window-vsplit)
  (global-set-key (kbd "C-, w W") 'evil-window-split)

  ;; arst
  (global-set-key (kbd "A-a")     'evil-backward-word-begin)
  (global-set-key (kbd "A-A")     'evil-backward-WORD-begin)
  (global-set-key (kbd "A-r")     'evil-forward-word-begin)
  (global-set-key (kbd "A-R")     'evil-forward-WORD-begin)
  (global-set-key (kbd "A-s")     'evil-forward-word-end)
  (global-set-key (kbd "A-S")     'evil-forward-WORD-end)
  (global-set-key (kbd "A-t")     'evil-goto-first-line)
  (global-set-key (kbd "A-T")     'evil-goto-line)

  ;; qwfp
  (global-set-key (kbd "A-q")     'evil-find-char)
  (global-set-key (kbd "A-Q")     'evil-find-char-backward)
  (global-set-key (kbd "A-w")     'evil-find-char-to)
  (global-set-key (kbd "A-W")     'evil-find-char-to-backward)
  (global-set-key (kbd "A-f")     'evil-forward-paragraph)
  (global-set-key (kbd "A-F")     'evil-backward-paragraph)
  (global-set-key (kbd "A-p")     'evil-scroll-page-down)
  (global-set-key (kbd "A-P")     'evil-scroll-page-up)

  ;; zxcv
  (global-set-key (kbd "A-z")     'evil-search-next)
  (global-set-key (kbd "A-Z")     'evil-search-previous)
  (global-set-key (kbd "A-x")     'evil-jump-forward)
  (global-set-key (kbd "A-X")     'evil-jump-backward)

  ;; <Tab>!@
  (global-set-key (kbd "<A-tab>") 'evil-jump-item)
  (global-set-key (kbd "A-1")     'evil-search-forward)
  (global-set-key (kbd "A-2")     'evil-search-backward)

  ;; neio'
  (global-set-key (kbd "A-n")     'evil-backward-char)
  (global-set-key (kbd "A-N")     'evil-beginning-of-visual-line)
  (global-set-key (kbd "A-e")     'evil-next-visual-line)
  (global-set-key (kbd "A-E")     'evil-window-bottom)
  (global-set-key (kbd "A-i")     'evil-previous-visual-line)
  (global-set-key (kbd "A-I")     'evil-window-top)
  (global-set-key (kbd "A-o")     'evil-forward-char)
  (global-set-key (kbd "A-O")     'evil-end-of-visual-line)
  (global-set-key (kbd "A-\"")    'evil-window-middle)
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
                   :func    #'serika-g/evil//operator-state-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'text-objects
                   :func    #'serika-g/evil//text-objects-keymap)

  (serika-c/eg/add :parents '("keymap evil")
                   :name    'ex-keymap
                   :func    #'serika-g/evil//ex-keymap))
