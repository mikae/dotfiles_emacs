;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika/evil/replace-buffer ()
  "Replace all text in current buffer with text from clipboard."
  (interactive)
  (erase-buffer)
  (x-clipboard-yank))

;; Global
(defun serika/evil//require ()
  "Require modules for `evil'."
  (require 'func-package)
  (require 'func-keymap)

  (require 'evil))

(defun serika/evil//settings ()
  "Configure `evil' variables."
  (setq evil-default-state 'emacs)

  (setq evil-insert-state-cursor    '((bar . 2) "yellow")
        evil-normal-state-cursor    '(box       "purple")
        evil-visual-state-cursor    '(box       "green")
        evil-motion-state-cursor    '((bar . 2) "orange")
        evil-replace-state-cursor   '(box       "red")
        evil-operator-state-cursor  '((bar . 2) "orange")
        evil-emacs-state-cursor     '(box       "cyan")))

(defun serika/evil//global-keymap ()
  "Configure global keymap."
)

(defun serika/evil//disable-mouse ()
  "Disable mouse in `evil'."
  (define-key evil-motion-state-map [down-mouse-1] 'ignore)
  (define-key evil-motion-state-map [drag-mouse-1] 'ignore)
  (define-key evil-motion-state-map [mouse-1]      'ignore)
  (define-key evil-normal-state-map [down-mouse-1] 'ignore)
  (define-key evil-normal-state-map [drag-mouse-1] 'ignore)
  (define-key evil-normal-state-map [mouse-1]      'ignore)
  (define-key evil-visual-state-map [down-mouse-1] 'ignore)
  (define-key evil-visual-state-map [drag-mouse-1] 'ignore)
  (define-key evil-visual-state-map [mouse-1]      'ignore)

  (define-key evil-motion-state-map [down-mouse-3] 'ignore)
  (define-key evil-motion-state-map [drag-mouse-3] 'ignore)
  (define-key evil-motion-state-map [mouse-3]      'ignore)
  (define-key evil-normal-state-map [down-mouse-3] 'ignore)
  (define-key evil-normal-state-map [drag-mouse-3] 'ignore)
  (define-key evil-normal-state-map [mouse-3]      'ignore)
  (define-key evil-visual-state-map [down-mouse-3] 'ignore)
  (define-key evil-visual-state-map [drag-mouse-3] 'ignore)
  (define-key evil-visual-state-map [mouse-3]      'ignore))

(defun serika/evil//normal-keymap ()
  "Configure `evil-normal-state-map'."
  (serika/keymap/unbind evil-normal-state-map)

  (define-key evil-normal-state-map (kbd "J") nil)

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
  (define-key evil-normal-state-map (kbd "H-s") 'serika/evil/replace-buffer)

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
  (define-key evil-normal-state-map (kbd "<C-m> C-e") 'evil-emacs-state))

(defun serika/evil//motion-keymap ()
  "Configure `evil-motion-state-map'."
  (serika/keymap/unbind evil-motion-state-map)
  (define-key evil-motion-state-map (kbd "G") nil)

  (define-key evil-motion-state-map [up] nil)
  (define-key evil-motion-state-map [right] nil)
  (define-key evil-motion-state-map [down] nil)
  (define-key evil-motion-state-map [left] nil)

  (define-key evil-motion-state-map (kbd "A-'") 'evil-goto-mark)

  ;; Bind digits to `digit-argument'
  )

(defun serika/evil//insert-keymap ()
  "Configure `evil-insert-state-map'."
  (serika/keymap/unbind evil-insert-state-map)

  ;; Redefine motions in `insert' state, because other state
  ;; expects `evil' motions
  (define-key evil-insert-state-map (kbd "A-h")   'evil-backward-char)
  (define-key evil-insert-state-map (kbd "A-l")   'evil-forward-char)

  ;; Ret with proper indentation
  (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

  ;; Exit insert state
  (define-key evil-insert-state-map (kbd "C-, C-j") 'evil-normal-state))

(defun serika/evil//visual-keymap ()
  "Configure `evil-visual-state-map'."
  (serika/keymap/unbind evil-visual-state-map)

  (define-key evil-visual-state-map "a" evil-outer-text-objects-map)
  (define-key evil-visual-state-map "i" evil-inner-text-objects-map)

  (define-key evil-visual-state-map (kbd "C-, C-j") 'evil-exit-visual-state))

(defun serika/evil//replace-keymap ()
  "Configure `evil-replace-state-map'."
  (serika/keymap/unbind evil-replace-state-map)

  (define-key evil-replace-state-map (kbd "C-, C-j") 'evil-normal-state))

(defun serika/evil//emacs-keymap ()
  "Configure `evil-emacs-state-map'."
  (serika/keymap/unbind evil-emacs-state-map)

  (define-key evil-emacs-state-map (kbd "<C-m> C-v") 'evil-normal-state))

(defun serika/evil//global-keymap ()
  "Add movements into Emacs global keymap."

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
  (serika/evil//require)
  (serika/evil//settings)
  (serika/evil//global-keymap)
  (serika/evil//disable-mouse)

  ;; Keymaps
  (serika/evil//normal-keymap)
  (serika/evil//motion-keymap)
  (serika/evil//insert-keymap)
  (serika/evil//visual-keymap)
  (serika/evil//replace-keymap)
  (serika/evil//emacs-keymap))
