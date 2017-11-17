;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/key//unset-bindings ()
  "Clear default bindings."
  (dolist (k '(
               "return" "tab" "escape"))
    (global-unset-key (kbd (concat "<C-" k ">")))
    (global-unset-key (kbd (concat "<H-" k ">")))
    (global-unset-key (kbd (concat "<A-" k ">")))

    (global-unset-key (kbd (concat "<C-H-" k ">")))
    (global-unset-key (kbd (concat "<A-C-" k ">")))
    (global-unset-key (kbd (concat "<C-M-" k ">")))
    (global-unset-key (kbd (concat "<A-H-" k ">")))
    (global-unset-key (kbd (concat "<H-M-" k ">")))
    (global-unset-key (kbd (concat "<A-M-" k ">")))

    (global-unset-key (kbd (concat "<A-C-H-" k ">")))
    (global-unset-key (kbd (concat "<C-H-M-" k ">")))
    (global-unset-key (kbd (concat "<A-H-M-" k ">")))
    (global-unset-key (kbd (concat "<A-C-M-" k ">")))

    (global-unset-key (kbd (concat "<A-C-H-M-" k ">"))))

  (dolist (k '("RET" "TAB" "ESC"))
    (global-unset-key (kbd k))
    (global-unset-key (kbd (concat "M-" k))))

  (dolist (k '(
               "q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
               "a" "s" "d" "f" "g" "h" "j" "k" "l"
               "z" "x" "c" "v" "b"  "n" "m"

               "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P"
               "A" "S" "D" "F" "G" "H" "J" "K" "L"
               "Z" "X" "C" "V" "B"  "N" "M"

               "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
               "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
               ";" ":" "[" "]" "{" "}" "," "<" "." ">" "/" "?" "'"))
    (global-unset-key (kbd (concat "C-" k)))
    (global-unset-key (kbd (concat "H-" k)))
    (global-unset-key (kbd (concat "A-" k)))
    (global-unset-key (kbd (concat "M-" k)))

    (global-unset-key (kbd (concat "C-H-" k)))
    (global-unset-key (kbd (concat "A-C-" k)))
    (global-unset-key (kbd (concat "C-M-" k)))
    (global-unset-key (kbd (concat "A-H-" k)))
    (global-unset-key (kbd (concat "H-M-" k)))
    (global-unset-key (kbd (concat "A-M-" k)))

    (global-unset-key (kbd (concat "A-C-H-" k)))
    (global-unset-key (kbd (concat "C-H-M-" k)))
    (global-unset-key (kbd (concat "A-H-M-" k)))
    (global-unset-key (kbd (concat "A-C-M-" k)))

    (global-unset-key (kbd (concat "A-C-H-M-" k)))))

(defun serika-g/key//disable-arrows ()
  "Disable arrows."
  (global-unset-key [up])
  (global-unset-key [right])
  (global-unset-key [down])
  (global-unset-key [left]))

(defun serika-g/key//clear-key-translation-map ()
  "Clear `key-translation-map'."
  (func/keymap/create key-translation-map))

(defun serika-g/key//clear-function-key-map ()
  "Clear `function-key-map'."
  ;; (func/keymap/create function-key-map)
  (dolist (--kbd '(
                   "C-@"
                   "C-x"
                   "<C-M-S-kp-0>"
                   "<C-M-S-kp-1>"
                   "<C-M-S-kp-2>"
                   "<C-M-S-kp-3>"
                   "<C-M-S-kp-4>"
                   "<C-M-S-kp-5>"
                   "<C-M-S-kp-6>"
                   "<C-M-S-kp-7>"
                   "<C-M-S-kp-8>"
                   "<C-M-S-kp-9>"
                   "<C-M-S-kp-add>"
                   "<C-M-S-kp-begin>"
                   "<C-M-S-kp-decimal>"
                   "<C-M-S-kp-delete>"
                   "<C-M-S-kp-divide>"
                   "<C-M-S-kp-down>"
                   "<C-M-S-kp-end>"
                   "<C-M-S-kp-enter>"
                   "<C-M-S-kp-home>"
                   "<C-M-S-kp-insert>"
                   "<C-M-S-kp-left>"
                   "<C-M-S-kp-multiply>"
                   "<C-M-S-kp-next>"
                   "<C-M-S-kp-prior>"
                   "<C-M-S-kp-right>"
                   "<C-M-S-kp-subtract>"
                   "<C-M-S-kp-up>"
                   "<C-M-kp-0>"
                   "<C-M-kp-1>"
                   "<C-M-kp-2>"
                   "<C-M-kp-3>"
                   "<C-M-kp-4>"
                   "<C-M-kp-5>"
                   "<C-M-kp-6>"
                   "<C-M-kp-7>"
                   "<C-M-kp-8>"
                   "<C-M-kp-9>"
                   "<C-M-kp-add>"
                   "<C-M-kp-begin>"
                   "<C-M-kp-decimal>"
                   "<C-M-kp-delete>"
                   "<C-M-kp-divide>"
                   "<C-M-kp-down>"
                   "<C-M-kp-end>"
                   "<C-M-kp-enter>"
                   "<C-M-kp-home>"
                   "<C-M-kp-insert>"
                   "<C-M-kp-left>"
                   "<C-M-kp-multiply>"
                   "<C-M-kp-next>"
                   "<C-M-kp-prior>"
                   "<C-M-kp-right>"
                   "<C-M-kp-subtract>"
                   "<C-M-kp-up>"
                   "<C-S-kp-0>"
                   "<C-S-kp-1>"
                   "<C-S-kp-2>"
                   "<C-S-kp-3>"
                   "<C-S-kp-4>"
                   "<C-S-kp-5>"
                   "<C-S-kp-6>"
                   "<C-S-kp-7>"
                   "<C-S-kp-8>"
                   "<C-S-kp-9>"
                   "<C-S-kp-add>"
                   "<C-S-kp-begin>"
                   "<C-S-kp-decimal>"
                   "<C-S-kp-delete>"
                   "<C-S-kp-divide>"
                   "<C-S-kp-down>"
                   "<C-S-kp-end>"
                   "<C-S-kp-enter>"
                   "<C-S-kp-home>"
                   "<C-S-kp-insert>"
                   "<C-S-kp-left>"
                   "<C-S-kp-multiply>"
                   "<C-S-kp-next>"
                   "<C-S-kp-prior>"
                   "<C-S-kp-right>"
                   "<C-S-kp-subtract>"
                   "<C-S-kp-up>"
                   "<C-kp-0>"
                   "<C-kp-1>"
                   "<C-kp-2>"
                   "<C-kp-3>"
                   "<C-kp-4>"
                   "<C-kp-5>"
                   "<C-kp-6>"
                   "<C-kp-7>"
                   "<C-kp-8>"
                   "<C-kp-9>"
                   "<C-kp-add>"
                   "<C-kp-begin>"
                   "<C-kp-decimal>"
                   "<C-kp-delete>"
                   "<C-kp-divide>"
                   "<C-kp-down>"
                   "<C-kp-end>"
                   "<C-kp-enter>"
                   "<C-kp-home>"
                   "<C-kp-insert>"
                   "<C-kp-left>"
                   "<C-kp-multiply>"
                   "<C-kp-next>"
                   "<C-kp-prior>"
                   "<C-kp-right>"
                   "<C-kp-subtract>"
                   "<C-kp-up>"
                   "<M-S-kp-0>"
                   "<M-S-kp-1>"
                   "<M-S-kp-2>"
                   "<M-S-kp-3>"
                   "<M-S-kp-4>"
                   "<M-S-kp-5>"
                   "<M-S-kp-6>"
                   "<M-S-kp-7>"
                   "<M-S-kp-8>"
                   "<M-S-kp-9>"
                   "<M-S-kp-add>"
                   "<M-S-kp-begin>"
                   "<M-S-kp-decimal>"
                   "<M-S-kp-delete>"
                   "<M-S-kp-divide>"
                   "<M-S-kp-down>"
                   "<M-S-kp-end>"
                   "<M-S-kp-enter>"
                   "<M-S-kp-home>"
                   "<M-S-kp-insert>"
                   "<M-S-kp-left>"
                   "<M-S-kp-multiply>"
                   "<M-S-kp-next>"
                   "<M-S-kp-prior>"
                   "<M-S-kp-right>"
                   "<M-S-kp-subtract>"
                   "<M-S-kp-up>"
                   "<M-kp-0>"
                   "<M-kp-1>"
                   "<M-kp-2>"
                   "<M-kp-3>"
                   "<M-kp-4>"
                   "<M-kp-5>"
                   "<M-kp-6>"
                   "<M-kp-7>"
                   "<M-kp-8>"
                   "<M-kp-9>"
                   "<M-kp-add>"
                   "<M-kp-begin>"
                   "<M-kp-decimal>"
                   "<M-kp-delete>"
                   "<M-kp-divide>"
                   "<M-kp-down>"
                   "<M-kp-end>"
                   "<M-kp-enter>"
                   "<M-kp-home>"
                   "<M-kp-insert>"
                   "<M-kp-left>"
                   "<M-kp-multiply>"
                   "<M-kp-next>"
                   "<M-kp-prior>"
                   "<M-kp-right>"
                   "<M-kp-subtract>"
                   "<M-kp-up>"
                   "<S-kp-0>"
                   "<S-kp-1>"
                   "<S-kp-2>"
                   "<S-kp-3>"
                   "<S-kp-4>"
                   "<S-kp-5>"
                   "<S-kp-6>"
                   "<S-kp-7>"
                   "<S-kp-8>"
                   "<S-kp-9>"
                   "<S-kp-add>"
                   "<S-kp-begin>"
                   "<S-kp-decimal>"
                   "<S-kp-delete>"
                   "<S-kp-divide>"
                   "<S-kp-down>"
                   "<S-kp-end>"
                   "<S-kp-enter>"
                   "<S-kp-home>"
                   "<S-kp-insert>"
                   "<S-kp-left>"
                   "<S-kp-multiply>"
                   "<S-kp-next>"
                   "<S-kp-prior>"
                   "<S-kp-right>"
                   "<S-kp-subtract>"
                   "<S-kp-up>"
                   "<S-tab>"
                   "<backspace>"
                   "<clear>"
                   "<delete>"
                   "<escape>"
                   "<kp-0>"
                   "<kp-1>"
                   "<kp-2>"
                   "<kp-3>"
                   "<kp-4>"
                   "<kp-5>"
                   "<kp-6>"
                   "<kp-7>"
                   "<kp-8>"
                   "<kp-9>"
                   "<kp-add>"
                   "<kp-begin>"
                   "<kp-decimal>"
                   "<kp-delete>"
                   "<kp-divide>"
                   "<kp-down>"
                   "<kp-end>"
                   "<kp-enter>"
                   "<kp-equal>"
                   "<kp-home>"
                   "<kp-insert>"
                   "<kp-left>"
                   "<kp-multiply>"
                   "<kp-next>"
                   "<kp-prior>"
                   "<kp-right>"
                   "<kp-separator>"
                   "<kp-space>"
                   "<kp-subtract>"
                   "<kp-tab>"
                   "<kp-up>"
                   "<left-fringe>"
                   "<linefeed>"
                   "<return>"
                   "<right-fringe>"
                   "<tab>"

                   "<right-fringe> <mouse-1>"
                   "<right-fringe> <mouse-2>"
                   "<right-fringe> <mouse-3>"

                   "<left-fringe> <mouse-1>"
                   "<left-fringe> <mouse-2>"
                   "<left-fringe> <mouse-3>"

                   "C-x @"

                   "C-x @ S"
                   "C-x @ a"
                   "C-x @ c"
                   "C-x @ h"
                   "C-x @ m"
                   "C-x @ s"))
    ;; (define-key function-key-map (kbd --kbd) nil)
    )
  )

(defun serika-g/key//configure-input-decode-map ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-o] [C-o])
  )

(defun serika-g/key//configure-outline-mode-map ()
  "Configure `outline-mode-map'"
  (func/keymap/save   outline-mode-map)
  (func/keymap/create outline-mode-map))

(defun serika-g/key//create-bindings ()
  "Create global bindings."
  (dolist (elem '("C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (global-set-key (kbd elem) 'digit-argument))

  ;; save/kill
  (func/keymap/define-global "C-x C-s"     #'save-buffer
                             "C-x C-S-s"   #'func/tramp/sudo-write
                             "C-x C-x C-s" #'func/buffer/invoke-save-function
                             "C-x C-c"     #'func/buffer/kill
                             "C-x C-q"     #'save-buffers-kill-terminal
                             "C-x C-x C-c" #'func/buffer/kill
                             "C-x C-h"     #'previous-buffer
                             "C-x C-g"     #'revert-buffer)

  ;; narrowing
  (func/keymap/define-global "C-x C-w"   #'widen)

  ;; toggles
  (func/keymap/define-global "C-x t r"   #'read-only-mode
                             "C-x t c"   #'serika-f/custom-modifiers/toggle)

  (func/keymap/define-global "C-x C-t v" #'yank)

  ;; describes
  (func/keymap/define-global "C-x h v"   #'describe-variable
                             "C-x h f"   #'describe-function
                             "C-x h k"   #'describe-key)

  ;; macro
  (func/keymap/define-global "<C-m> n"   #'kmacro-start-macro-or-insert-counter
                             "<C-m> r"   #'kmacro-end-or-call-macro)

  ;; misc
  (func/keymap/define-global "C-g" #'keyboard-quit)

  (func/keymap/define-global "M-r"       #'eval-expression)

  ;; (cm-define-global "control-q" #'func/buffer/kill)
  )

(defun init ()
  "Configure keys."
  (serika-c/eg/add-many-by-parents ("global-keymap")
    'unset-bindings
    #'serika-g/key//unset-bindings

    'clear-key-translation-map
    #'serika-g/key//clear-key-translation-map

    'clear-function-key-map
    #'serika-g/key//clear-function-key-map

    'disable-arrows
    #'serika-g/key//disable-arrows

    'input-decode-map
    #'serika-g/key//configure-input-decode-map

    'configure-outline-mode-map
    #'serika-g/key//configure-outline-mode-map

    'create-new
    #'serika-g/key//create-bindings))
