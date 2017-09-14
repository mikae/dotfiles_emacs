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

(defun serika-g/key//configure-input-decode-map ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-o] [C-o]))

(defun serika-g/key//create-bindings ()
  "Create global bindings."
  (dolist (elem '("C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (global-set-key (kbd elem) 'digit-argument))

  ;; save/kill ... etc
  (func/keymap/define-global "C-x C-s"     #'save-buffer
                             "C-x C-S-s"   #'func/tramp/sudo-write
                             "C-x C-x C-s" #'func/buffer/invoke-save-function
                             "C-x C-c"     #'func/buffer/kill
                             "C-x C-x C-c" #'func/buffer/kill
                             "C-x C-h"     #'previous-buffer
                             "C-x C-g"     #'revert-buffer)

  ;; narrowing
  (func/keymap/define-global "C-x C-q"   #'save-buffers-kill-terminal
                             "C-x C-w"   #'widen)

  ;; toggles
  (func/keymap/define-global "C-x t r"   #'read-only-mode)

  ;; describes
  (func/keymap/define-global "C-x h v"   #'describe-variable
                             "C-x h f"   #'describe-function
                             "C-x h k"   #'describe-key)

  ;; changes
  (func/keymap/define-global "C-x c u"   #'serika-f/settings/change-user)

  ;; macro
  (func/keymap/define-global "<C-m> n"   #'kmacro-start-macro-or-insert-counter
                             "<C-m> r"   #'kmacro-end-or-call-macro)

  ;; windows
  (func/keymap/define-global "C-w f"     #'delete-other-windows
                             "C-w F"     #'serika-f/window/delete-but-scratch)

  (func/keymap/define-global "M-r"       #'eval-expression))

(defun init ()
  "Configure keys."
  (serika-c/eg/add-many-by-parents ("global-keymap")
                                   'unset-bindings
                                   #'serika-g/key//unset-bindings

                                   'clear-key-translation-map
                                   #'serika-g/key//clear-key-translation-map

                                   'disable-arrows
                                   #'serika-g/key//disable-arrows

                                   'input-decode-map
                                   #'serika-g/key//configure-input-decode-map

                                   'create-new-bindings
                                   #'serika-g/key//create-bindings))