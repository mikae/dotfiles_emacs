;;; package --- Summary
;;; Commentary:
;;; Code:

;; Private
(defun func/keymap//resolve-binding (func)
  "Resolves FUNC."
  (cond
   ((null func)
    nil)
   ((symbolp func)
    (let ((--symbol-value (symbol-value func)))
      (if (keymapp --symbol-value)
          --symbol-value
        nil)))
   ((or (eq (car func)
            'function)
        (eq (car func)
            'quote))
    (nth 1 func))
   ((eq (car func)
        'lambda)
    func)
   ((functionp (car func))
    (apply (car func) (cdr func)))
   ((macrop (car func))
    (macroexpand-all func))
   (t (error "Unusual arg"))))

(defun func/keymap//bind (map arg func)
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

(defun func/keymap//create-bindings (keymap args)
  (cl-loop for --kbd  in args       by #'cddr
           for --func in (cdr args) by #'cddr
           do
           (func/keymap//bind keymap --kbd (func/keymap//resolve-binding --func))))

;; Functions
(defun func/keymap/bind-digits (map func)
  "Bind FUNC to digits in MAP."
  (func/keymap//bind map '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") func))

(defmacro func/keymap/create (keymap-name &rest args)
  "Create new KEYMAP-NAME.
Example:
(func/keymap/create some-keymap
                    some-binding 'func                     ;;1
                    some-binding #'func                    ;;2
                    some-binding (lambda () (interactive)
                                   some-actions)  ;;3
                    some-binding (some-macro some-args)    ;;4
                    some-binding (some-function some-args) ;;5
                    )
1, 2, 3 - will be passed as usually
4 will be expanded and passed
5 wiii be executed and passed
"
  `(when (cl-evenp (length ',args))
     (setq ,keymap-name (make-sparse-keymap))
     (func/keymap//create-bindings ,keymap-name ',args)))

(defmacro func/keymap/define (keymap-name &rest args)
  "Add bindings to MAP. Example:
Example:
(func/keymap/create some-keymap
                    some-binding 'func                     ;;1
                    some-binding #'func                    ;;2
                    some-binding (lambda () (interactive)
                                   some-actions)  ;;3
                    some-binding (some-macro some-args)    ;;4
                    some-binding (some-function some-args) ;;5
                    )
1, 2, 3 - will be passed as usually
4 will be expanded and passed
5 wiii be executed and passed
"
`(when (cl-evenp (length ',args))
   (func/keymap//create-bindings ,keymap-name ',args)))

(defmacro func/keymap/define-global (&rest args)
  "Add bindings to `global-map'. Example:
Example:
(func/keymap/create some-keymap
                    some-binding 'func                     ;;1
                    some-binding #'func                    ;;2
                    some-binding (lambda () (interactive)
                                   some-actions)  ;;3
                    some-binding (some-macro some-args)    ;;4
                    some-binding (some-function some-args) ;;5
                    )
1, 2, 3 - will be passed as usually
4 will be expanded and passed
5 wiii be executed and passed
"
 `(when (cl-evenp (length ',args))
    (func/keymap//create-bindings global-map ',args)))

(defmacro func/keymap/save (&rest keymaps)
  `(dolist (--keymap ',keymaps)
     (set (intern (concat "--func-saved-keymap-"
                          (symbol-name --keymap)))
          (symbol-value --keymap))))

(provide 'func-keymap)
;;; func-keymap.el ends here
