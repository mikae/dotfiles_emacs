;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/key//require ()
  "Require modules for `dired'."
  (require 'func-buffer)
  (require 'func-tramp)
  (require 'func-window))

(defun serika-g/key//unset-bindings ()
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

(defun serika-g/key//disable-arrows ()
  "Disable arrows."
  (global-unset-key [up])
  (global-unset-key [right])
  (global-unset-key [down])
  (global-unset-key [left]))

(defun serika-g/key//configure-input-decode-map ()
  "Configure `input-decode-map'."
  (define-key input-decode-map [?\C-m] [C-m]))

(defun serika-g/key//create-bindings ()
  "Create global bindings."
  (dolist (elem '("C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
    (global-set-key (kbd elem) 'digit-argument))

  (global-set-key (kbd "C-x C-s")   #'serika-f/buffer/save)
  (global-set-key (kbd "C-x C-S-s") #'serika-f/tramp/sudo-write)
  (global-set-key (kbd "C-x C-c")   #'serika-f/buffer/kill)
  (global-set-key (kbd "C-x C-h")   #'serika-f/buffer/hide)
  (global-set-key (kbd "C-x C-g")   #'revert-buffer)

  (global-set-key (kbd "C-x C-q")   #'save-buffers-kill-terminal)

  (global-set-key (kbd "M-a")       #'eval-expression)

  ;; Describe
  (global-set-key (kbd "C-x h v")   #'describe-variable)
  (global-set-key (kbd "C-x h f")   #'describe-function)
  (global-set-key (kbd "C-x h k")   #'describe-key)

  ;; Transform
  (global-set-key (kbd "C-t w")     #'delete-trailing-whitespace)

  ;; Toggle
  (global-set-key (kbd "C-x t r")   #'read-only-mode)

  ;; Windows
  (global-set-key (kbd "C-, w f")   #'delete-other-windows)
  (global-set-key (kbd "C-, w F")   #'serika-f/window/only-new-window))

(defun init ()
  "Configure keys."
  (serika-c/eg/add :parents '("base require")
                   :name    'keys
                   :func    #'serika-g/key//require)

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
