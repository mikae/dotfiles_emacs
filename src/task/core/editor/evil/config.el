(require 'func-package)
(require 'func-misc)

;; Install `evil'
(serika/package/make-sure-installed 'evil)

;; Configure `evil'
(require 'evil)

;; (evil-mode 1)
(setq evil-default-state 'emacs)

(setq evil-insert-state-cursor    '((bar . 2) "yellow")
      evil-normal-state-cursor    '(box       "purple")
      evil-visual-state-cursor    '(box       "green")
      evil-motion-state-cursor    '((bar . 2) "orange")
      evil-replace-state-cursor   '(box       "red")
      evil-operator-state-cursor  '((bar . 2) "orange")
      evil-emacs-state-cursor     '(box "cyan"))

(global-set-key (kbd "C-, w k") 'evil-window-up)
(global-set-key (kbd "C-, w l") 'evil-window-right)
(global-set-key (kbd "C-, w j") 'evil-window-down)
(global-set-key (kbd "C-, w h") 'evil-window-left)
(global-set-key (kbd "C-, w w") 'evil-window-vsplit)
(global-set-key (kbd "C-, w W") 'evil-window-split)
(global-set-key (kbd "C-, w q") 'evil-window-delete)

(define-key evil-motion-state-map [down-mouse-1] 'serika/misc/silence)
(define-key evil-motion-state-map [drag-mouse-1] 'serika/misc/silence)
(define-key evil-motion-state-map [mouse-1]      'serika/misc/silence)
(define-key evil-normal-state-map [down-mouse-1] 'serika/misc/silence)
(define-key evil-normal-state-map [drag-mouse-1] 'serika/misc/silence)
(define-key evil-normal-state-map [mouse-1]      'serika/misc/silence)
(define-key evil-visual-state-map [down-mouse-1] 'serika/misc/silence)
(define-key evil-visual-state-map [drag-mouse-1] 'serika/misc/silence)
(define-key evil-visual-state-map [mouse-1]      'serika/misc/silence)

(define-key evil-motion-state-map [down-mouse-3] 'serika/misc/silence)
(define-key evil-motion-state-map [drag-mouse-3] 'serika/misc/silence)
(define-key evil-motion-state-map [mouse-3]      'serika/misc/silence)
(define-key evil-normal-state-map [down-mouse-3] 'serika/misc/silence)
(define-key evil-normal-state-map [drag-mouse-3] 'serika/misc/silence)
(define-key evil-normal-state-map [mouse-3]      'serika/misc/silence)
(define-key evil-visual-state-map [down-mouse-3] 'serika/misc/silence)
(define-key evil-visual-state-map [drag-mouse-3] 'serika/misc/silence)
(define-key evil-visual-state-map [mouse-3]      'serika/misc/silence)

(serika/misc/unbind-keys evil-normal-state-map)

(define-key evil-normal-state-map (kbd "J") nil)

(define-key evil-normal-state-map (kbd "a")   'evil-append)
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
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)

(define-key evil-normal-state-map (kbd "u")   'undo)
(define-key evil-normal-state-map (kbd "U")   'redo)
(define-key evil-normal-state-map (kbd "\"")  'evil-use-register)
(define-key evil-normal-state-map (kbd "\"")  'evil-use-register)

(define-key evil-normal-state-map (kbd "A-w") 'delete-trailing-whitespace)

(define-key evil-normal-state-map (kbd "v" )  'evil-visual-char)
(define-key evil-normal-state-map (kbd "V" )  'evil-visual-line)
(define-key evil-normal-state-map (kbd "A-v") 'evil-visual-block)

;; Macro
(define-key evil-normal-state-map (kbd "q")   'kmacro-start-macro-or-insert-counter)
(define-key evil-normal-state-map (kbd "A-q") 'kmacro-end-or-call-macro)

(define-key evil-normal-state-map (kbd "<C-m> C-e") 'evil-emacs-state)

(serika/misc/unbind-keys evil-motion-state-map)

(define-key evil-motion-state-map [up] nil)
(define-key evil-motion-state-map [right] nil)
(define-key evil-motion-state-map [down] nil)
(define-key evil-motion-state-map [left] nil)

(define-key evil-motion-state-map (kbd "0")   'digit-argument)
(define-key evil-motion-state-map (kbd "1")   'digit-argument)
(define-key evil-motion-state-map (kbd "2")   'digit-argument)
(define-key evil-motion-state-map (kbd "3")   'digit-argument)
(define-key evil-motion-state-map (kbd "4")   'digit-argument)
(define-key evil-motion-state-map (kbd "5")   'digit-argument)
(define-key evil-motion-state-map (kbd "6")   'digit-argument)
(define-key evil-motion-state-map (kbd "7")   'digit-argument)
(define-key evil-motion-state-map (kbd "8")   'digit-argument)
(define-key evil-motion-state-map (kbd "9")   'digit-argument)
(define-key evil-motion-state-map (kbd "g")   'evil-goto-first-line)
(define-key evil-motion-state-map (kbd "G")   'evil-goto-line)
(define-key evil-motion-state-map (kbd "b")   'evil-backward-word-begin)
(define-key evil-motion-state-map (kbd "B")   'evil-backward-WORD-begin)
(define-key evil-motion-state-map (kbd "e")   'evil-forward-word-end)
(define-key evil-motion-state-map (kbd "E")   'evil-forward-WORD-end)
(define-key evil-motion-state-map (kbd "f")   'evil-find-char)
(define-key evil-motion-state-map (kbd "F")   'evil-find-char-backward)
(define-key evil-motion-state-map (kbd "j")   'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k")   'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "h")   'evil-backward-char)
(define-key evil-motion-state-map (kbd "l")   'evil-forward-char)
(define-key evil-motion-state-map (kbd "t")   'evil-find-char-to)
(define-key evil-motion-state-map (kbd "T")   'evil-find-char-to-backward)
(define-key evil-motion-state-map (kbd "w")   'evil-forward-word-begin)
(define-key evil-motion-state-map (kbd "W")   'evil-forward-WORD-begin)
(define-key evil-motion-state-map (kbd "{")   'evil-backward-paragraph)
(define-key evil-motion-state-map (kbd "}")   'evil-forward-paragraph)
(define-key evil-motion-state-map (kbd "%")   'evil-jump-item)
(define-key evil-motion-state-map (kbd "C-'") 'evil-goto-mark)
(define-key evil-motion-state-map (kbd "'")   'evil-goto-mark-line)
(define-key evil-motion-state-map (kbd "*")   'evil-search-word-forward)
(define-key evil-motion-state-map (kbd "/")   'evil-search-forward)
(define-key evil-motion-state-map (kbd "?")   'evil-search-backward)

(define-key evil-motion-state-map (kbd "A-O")  'evil-jump-forward)
(define-key evil-motion-state-map (kbd "A-o")  'evil-jump-backward)

(define-key evil-motion-state-map (kbd "A-h")  'evil-beginning-of-visual-line)
(define-key evil-motion-state-map (kbd "A-j")  'evil-window-bottom)
(define-key evil-motion-state-map (kbd "A-k")  'evil-window-top)
(define-key evil-motion-state-map (kbd "A-l")  'evil-end-of-visual-line)
(define-key evil-motion-state-map (kbd "A-m")  'evil-window-middle)

(define-key evil-motion-state-map (kbd "A-n")  'evil-search-next)
(define-key evil-motion-state-map (kbd "A-N")  'evil-search-previous)

(define-key evil-motion-state-map (kbd "A-f")  'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "A-b")  'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "A-d")  'evil-scroll-down)
(define-key evil-motion-state-map (kbd "A-u")  'evil-scroll-up)

(serika/misc/unbind-keys evil-insert-state-map)

(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

(define-key evil-insert-state-map (kbd "C-, C-j") 'evil-normal-state)

(serika/misc/unbind-keys evil-visual-state-map)

(define-key evil-visual-state-map "a" evil-outer-text-objects-map)
(define-key evil-visual-state-map "i" evil-inner-text-objects-map)

(define-key evil-visual-state-map (kbd "C-, C-j") 'evil-exit-visual-state)

(serika/misc/unbind-keys evil-replace-state-map)

(define-key evil-replace-state-map (kbd "C-, C-j") 'evil-normal-state)

(serika/misc/unbind-keys evil-emacs-state-map)

(define-key evil-emacs-state-map (kbd "<C-m> C-v") 'evil-normal-state)
