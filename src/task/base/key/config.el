;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-buffer)

(funcall (lambda ()
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

    (global-unset-key (kbd (concat "A-C-H-M-" k))))))

(funcall (lambda ()
  "Disable arrows."
  (global-unset-key [up])
  (global-unset-key [right])
  (global-unset-key [down])
  (global-unset-key [left])))

(funcall (lambda ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m])))

(funcall (lambda ()
  "Add global bindings."
  (global-set-key (kbd "C-x C-s") 'serika/buffer/save)
  (global-set-key (kbd "C-x C-c") 'serika/buffer/kill)
  (global-set-key (kbd "C-x C-h") 'serika/buffer/hide)
  (global-set-key (kbd "C-x C-r") 'revert-buffer)

  (global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)

  (global-set-key (kbd "A-:")     'eval-expression)

  (global-set-key (kbd "C-h v")   'describe-variable)
  (global-set-key (kbd "C-h f")   'describe-function)
  (global-set-key (kbd "C-h k")   'describe-key)))
