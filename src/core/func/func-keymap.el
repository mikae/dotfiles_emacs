;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/keymap/unbind (target-keymap)
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

(defun serika-f/keymap/bind (map arg func)
  "If ARG is a list, bind FUNC to all elements of ARG.
If ARG is a string, bind FUNC to kbd of ARG.
Otherwise, throw an error."
  (if (not (null map))
      (cond ((null arg) (error "ARG mustn't be nil"))
             ((listp arg) (dolist (elem arg)
                            (define-key map (kbd elem) func)))
             ((stringp arg) (define-key map (kbd arg)  func))
             (t (error "ARG must be list or string")))
    (error "MAP mustn't be nil")))

(defun serika-f/keymap/bind-digits (map func)
  "Bind FUNC to digits in MAP."
  (serika-f/keymap/bind map '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") func))

(provide 'func-keymap)
;;; func-keymap.el ends here
