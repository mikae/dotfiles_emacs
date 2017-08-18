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

(defun serika-g/key//configure-input-decode-map ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-o] [C-o]))

(defun serika-g/key//create-bindings ()
  "Create global bindings."
  (dolist (elem '("C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (global-set-key (kbd elem) 'digit-argument))

  (func/keymap/define-global "C-x C-s"   #'save-buffer
                             "C-x C-S-s" #'func/tramp/sudo-write
                             "C-x C-c"   #'func/buffer/kill
                             "C-x C-h"   #'previous-buffer
                             "C-x C-g"   #'revert-buffer

                             "C-x C-q"   #'save-buffers-kill-terminal

                             "C-x t r"   #'read-only-mode

                             "C-x m n"   #'kmacro-start-macro-or-insert-counter
                             "C-x m r"   #'kmacro-end-or-call-macro

                             "C-x h v"   #'describe-variable
                             "C-x h f"   #'describe-function
                             "C-x h k"   #'describe-key

                             "C-x c u"   #'serika-f/settings/change-user

                             "C-t w"     #'delete-trailing-whitespace

                             "C-w f"     #'delete-other-windows
                             "C-w F"     #'func/window/only-new-window

                             "M-r"       #'eval-expression))

(defun init ()
  "Configure keys."
  (serika-c/eg/add :parents '("global-keymap")
                   :name    'unset-bindings
                   :func    #'serika-g/key//unset-bindings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'disable-arrows
                   :func    #'serika-g/key//disable-arrows)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'input-decode-map
                   :func    #'serika-g/key//configure-input-decode-map)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'create-new-bindings
                   :func    #'serika-g/key//create-bindings))
