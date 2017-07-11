;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika/keymap/unbind (target-keymap)
  "Unbind all keymappings from TARGET-KEYMAP."
  (dolist (k '("return" "tab" "escape"))
    (define-key target-keymap (kbd (concat "<C-" k ">")) nil)
    (define-key target-keymap (kbd (concat "<C-M-" k ">")) nil))

  (dolist (k '("RET" "TAB" "ESC"))
    (define-key target-keymap (kbd k) nil)
    (define-key target-keymap (kbd (concat "M-" k)) nil))

  (dolist (k '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
               "a" "s" "d" "f" "g" "h" "j" "k" "l"
               "z" "x" "c" "v" "b"  "n" "m"
               "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
               "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
               ";" ":" "[" "]" "{" "}" "," "<" "." ">" "/" "?" "'"))
    (define-key target-keymap (kbd k)                 nil)
    (define-key target-keymap (kbd (concat "C-" k))   nil)
    (define-key target-keymap (kbd (concat "M-" k))   nil)
    (define-key target-keymap (kbd (concat "C-M-" k)) nil)))

(provide 'func-keymap)
;;; func-keymap.el ends here
