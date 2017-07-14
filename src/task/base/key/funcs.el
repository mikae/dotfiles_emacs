;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/key//require ()
  "Require modules for `dired'."
  (require 'func-buffer)
  (require 'func-tramp))

(defun serika/key//unset-bindings ()
  "Clear default bindings."
  (message "lol")
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

(defun serika/key//disable-arrows ()
  "Disable arrows."
  (global-unset-key [up])
  (global-unset-key [right])
  (global-unset-key [down])
  (global-unset-key [left]))

(defun serika/key//configure-input-decode-map ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m]))

(defun serika/key//create-bindings ()
  "Create global bindings."
  (dolist (elem '("C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (global-set-key (kbd elem) 'digit-argument))

  (global-set-key (kbd "C-x s") #'serika/buffer/save)
  (global-set-key (kbd "C-x S") #'serika/tramp/sudo-write)
  (global-set-key (kbd "C-x c") #'serika/buffer/kill)
  (global-set-key (kbd "C-x h") #'serika/buffer/hide)
  (global-set-key (kbd "C-x r") #'revert-buffer)

  (global-set-key (kbd "C-x q") #'save-buffers-kill-terminal)

  (global-set-key (kbd "C-:")   #'eval-expression)

  (global-set-key (kbd "C-h v") #'describe-variable)
  (global-set-key (kbd "C-h f") #'describe-function)
  (global-set-key (kbd "C-h k") #'describe-key)

  ;; Transform
  (global-set-key (kbd "C-t w") #'delete-trailing-whitespace)

  ;; Toggle
  (global-set-key (kbd "C-T r") #'read-only-mode))

(defun init ()
  "Configure keys."
  (serika/key//require)
  (serika/key//unset-bindings)
  (serika/key//disable-arrows)
  (serika/key//configure-input-decode-map)
  (serika/key//create-bindings))
