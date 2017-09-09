;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/smartparens/next-paren (&optional closing)
  "Go to the next/previous closing/opening parenthesis/bracket/brace."
  (if closing
      (let ((curr (point)))
        (forward-char)
        (unless (eq curr (search-forward-regexp "[])}]"))
          (backward-char)))
    (search-backward-regexp "[[({]")))

(defun serika-f/smartparens/backward-opening-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (serika-f/smartparens/next-paren))

(defun serika-f/smartparens/forward-closing-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (serika-f/smartparens/next-paren :closing))

(defun serika-f/smartparens/forward-symbol (&optional count)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol (+ (or count 0) n))
    (sp-backward-symbol)))

(defun serika-f/smartparens/yank-symbol (&optional count)
  "Yank COUNT symbols."
  (interactive "P")
  (error "Complete me pls :(")
  (evil-visual-char)
  (serika-f/smartparens/forward-symbol (or count 1))
  (evil-yank))

(defun serika-f/smartparens/change-symbol (&optional count)
  "Change COUNT symbols."
  (interactive "P")
  (error "Complete me pls :(")
  (evil-visual-char)
  (serika-f/smartparens/forward-symbol (or count 1))
  (evil-delete (region-beginning) (region-end))
  (evil-insert 1))

(defun serika-f/smartparens/change-sexp (&optional count)
  "Change COUNT symbols."
  (interactive "P")
  (error "Complete me pls :(")
  (evil-visual-char)
  (sp-mark-sexp)
  (evil-delete (region-beginning) (region-end))
  (evil-insert 1))

(defun serika-f/smartparens/activate ()
  "Enable `smartparens' in the current buffer."
  (smartparens-mode +1))

;; Init
(defun init ()
  "Configure smartparens."
  (serika-c/eg/add-install :package-list '(smartparens)
                           :name 'smartparens)

  (serika-c/eg/add-many-by-name 'smartparens
                                ("require")
                                (lambda ()
                                  (require 'smartparens))

                                ("settings")
                                (lambda ()
                                  (sp-pair "\\\\(" nil :actions :rem)
                                  (sp-pair "\\{"   nil :actions :rem)
                                  (sp-pair "\\("   nil :actions :rem)
                                  (sp-pair "\\\""  nil :actions :rem)
                                  (sp-pair "/*"    nil :actions :rem)
                                  (sp-pair "\""    nil :actions :rem)
                                  (sp-pair "'"     nil :actions :rem)
                                  (sp-pair "("     nil :actions :rem)
                                  (sp-pair "["     nil :actions :rem)
                                  (sp-pair "{"     nil :actions :rem)
                                  (sp-pair "`"     nil :actions :rem))

                                ("settings evil")
                                (lambda ()
                                  (evil-define-state sexp
                                    "State for editing S-expressions."
                                    :tag "<Lsp>"
                                    :suppress-keymap t))

                                ("keymap evil")
                                (lambda ()
                                  (func/keymap/define evil-sexp-state-map
                                                      ;;qwfpg
                                                      "q" #'sp-splice-sexp
                                                      "Q" #'sp-splice-sexp-killing-around
                                                      "w" #'sp-splice-sexp-killing-forward
                                                      "W" #'sp-splice-sexp-killing-backward
                                                      ;; "f"
                                                      ;; "F"
                                                      ;; "p"
                                                      ;; "P"
                                                      ;; "g"
                                                      ;; "G"

                                                      ;;arstd
                                                      "a" #'sp-push-hybrid-sexp
                                                      "A" #'sp-absorb-sexp
                                                      "r" #'sp-join-sexp
                                                      "R" #'sp-convolute-sexp
                                                      "s" #'sp-forward-barf-sexp
                                                      "S" #'sp-backward-barf-sexp
                                                      ;; "t"
                                                      ;; "T"
                                                      ;; "d"
                                                      ;; "D" #'sp-join-sexp

                                                      ;;neio
                                                      "n" #'sp-backward-symbol
                                                      "e" #'serika-f/smartparens/forward-closing-paren
                                                      "i" #'serika-f/smartparens/backward-opening-paren
                                                      "o" #'serika-f/smartparens/forward-symbol

                                                      "N" #'sp-backward-sexp
                                                      "E" #'ignore
                                                      "I" #'ignore
                                                      "O" #'sp-forward-sexp

                                                      ;;zxcvb
                                                      "z" #'sp-kill-symbol
                                                      "Z" #'sp-kill-sexp
                                                      "x" #'serika-f/smartparens/yank-symbol
                                                      "X" #'sp-copy-sexp
                                                      "c" #'serika-f/smartparens/change-symbol
                                                      "C" #'serika-f/smartparens/change-sexp

                                                      "SPC" #'evil-change-to-previous-state))))
