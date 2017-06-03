;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun serika/misc/silence ()
  "Do nothing."
  (interactive))

(defun serika/misc/unbind-keys (target-keymap)
  "Unbind keymaps from TARGET-KEYMAP."
  (dolist (k '(
               "return" "tab" "escape"))
    (define-key target-keymap (kbd (concat "<C-" k ">")) nil)
    (define-key target-keymap (kbd (concat "<C-M-" k ">")) nil))

  (dolist (k '(
               "RET" "TAB" "ESC"))
    (define-key target-keymap (kbd k) nil)
    (define-key target-keymap (kbd (concat "M-" k)) nil))

  (dolist (k '(
               "q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
               "a" "s" "d" "f" "g" "h" "j" "k" "l"
               "z" "x" "c" "v" "b"  "n" "m"
               "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
               "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
               ";" ":" "[" "]" "{" "}" "," "<" "." ">" "/" "?" "'"))
    (define-key target-keymap (kbd k)                 nil)
    (define-key target-keymap (kbd (concat "C-" k))   nil)
    (define-key target-keymap (kbd (concat "M-" k))   nil)
    (define-key target-keymap (kbd (concat "C-M-" k)) nil)))

(defun serika/misc/eval-file (file)
  "Execute FILE and return the result of the last expression."
  (load-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (goto-char (point-max))
    (backward-sexp)
    (eval (sexp-at-point))))

(provide 'func-misc)
;;; func-misc.el ends here
