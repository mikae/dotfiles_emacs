;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/keymap/p (arg)
  "Return t if ARG is a keymap."
  (and (listp arg)
       (> (length arg)
          0)
       (eq (car arg)
           'keymap)))

(defun serika-f/keymap/bind (map arg func)
  "If ARG is a list, bind FUNC to all elements of ARG.
If ARG is a string, bind FUNC to kbd of ARG.
Otherwise, throw an error."
  (if (not (null map))
      (cond ((null arg) (error "ARG mustn't be nil"))
            ((listp arg) (dolist (elem arg)
                            (define-key map (kbd elem) func)))
            ((vectorp arg) (define-key map arg func))
            ((stringp arg) (define-key map (kbd arg) func))
            (t (error "ARG must be list or string")))
    (error "MAP mustn't be nil")))

(defun serika-f/keymap/bind-digits (map func)
  "Bind FUNC to digits in MAP."
  (serika-f/keymap/bind map '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") func))

(defun serika-f/keymap/create (&rest args)
  "Create new keymap with bindings. Example:
(serika-f/keymap/create kbd-1 func-1
                        kbd-2 func-2)"
  (when (= (% (length args)
              2)
           1)
    (error "Invalid argument count"))
  (let ((map     (make-sparse-keymap))
        (counter 0)
        (length  (length args)))
    (while (< counter
              length)
      (serika-f/keymap/bind map
                            (nth counter args)
                            (nth (1+ counter) args))
      (setq counter (+ counter 2)))
      map))

(defun serika-f/keymap/define (map &rest args)
  "Add bindings to MAP. Example:
(serika-f/keymap/define map
                        kbd-1 func-1
                        kbd-2 func-2)"
  (let ((counter 0)
        (length  (length args)))
    (while (< counter
              length)
      (serika-f/keymap/bind map
                            (nth counter args)
                            (nth (1+ counter) args))
      (setq counter (+ counter 2)))
      map))

(defun serika-f/keymap/define-global (&rest args)
  "Add bindings to `global-map'. Example:
(serika-f/keymap/define kbd-1 func-1
                        kbd-2 func-2)"
  (let ((counter 0)
        (length  (length args)))
    (while (< counter
              length)
      (global-set-key (kbd (nth counter args))
                      (nth (1+ counter) args))
      (setq counter (+ counter 2)))))

(provide 'func-keymap)
;;; func-keymap.el ends here
