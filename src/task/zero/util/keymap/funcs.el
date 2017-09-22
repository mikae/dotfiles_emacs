;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Variables
(defvar funcs-keymap-saved-pattern "--func-saved-keymap-%s"
  "Pattern for saved keymaps.")

;; Private
(defun func/keymap//resolve-binding (func)
  "Resolves FUNC."
  (cond
   ((null func)
    nil)
   ((symbolp func)
    (let ((--symbol-value (symbol-value func)))
      (cond
       ((keymapp --symbol-value)
        --symbol-value)
       (t func))))
   ((listp func)
    (cond
     ((or (eq (car func)
              'function)
          (eq (car func)
              'quote))
      (nth 1 func))
     ((eq (car func)
          'lambda)
      ;; because (functionp 'lambda) returns nil
      (eval func))
     ((functionp (car func))
      (lambda ()
        (interactive)
        (eval func)))
     ((macrop (car func))
      (let ((--macro-expansion (macroexpand-all func)))
        (lambda ()
          (interactive)
          (eval --macro-expansion)))
      )))
   (t (error "func/keymap//resolve-binding: unusual arg: %s" func))))

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
  (unless (keymapp keymap)
    (error "func/keymap//create-bindings: KEYMAP is not a valid keymap"))
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
                                   some-actions)           ;;3
                    some-binding (some-macro some-args)    ;;4
                    some-binding (some-function some-args) ;;5
                    )
1, 2, 3 - will be passed as usually
4 will be expanded and wrapped into a lambda
5 wiii be wrapped into a lambda
"
  `(when (cl-evenp (length ',args))
     (setq ,keymap-name (make-sparse-keymap))
     (func/keymap//create-bindings ,keymap-name ',args)))

(defmacro func/keymap/define (keymap-name &rest args)
  "Add bindings to MAP. Example:
Example:
(func/keymap/define some-keymap
                    some-binding 'func                     ;;1
                    some-binding #'func                    ;;2
                    some-binding (lambda ()
                                   (interactive)
                                   some-actions)           ;;3
                    some-binding (some-macro some-args)    ;;4
                    some-binding (some-function some-args) ;;5
                    )
1, 2, 3 - will be passed as usually
4 will be expanded and wrapped into a lambda
5 wiii be wrapped into a lambda
"
`(when (cl-evenp (length ',args))
   (func/keymap//create-bindings ,keymap-name ',args)))

(defmacro func/keymap/define-global (&rest args)
  "Add bindings to `global-map'. Example:
Example:
(func/keymap/define-global some-keymap
                           some-binding 'func                     ;;1
                           some-binding #'func                    ;;2
                           some-binding (lambda ()
                                          (interactive)
                                          some-actions)           ;;3
                           some-binding (some-macro some-args)    ;;4
                           some-binding (some-function some-args) ;;5
                           )
1, 2, 3 - will be passed as usually
4 will be expanded and wrapped into a lambda
5 wiii be wrapped into a lambda
"
 `(when (cl-evenp (length ',args))
    (func/keymap//create-bindings global-map ',args)))

(defmacro func/keymap/save (&rest keymaps)
  (dolist (--keymap keymaps)
    (unless (keymapp (symbol-value --keymap))
      (error "")))
  `(dolist (--keymap ',keymaps)
     (set (intern (format funcs-keymap-saved-pattern
                          (symbol-name --keymap)))
          (symbol-value --keymap))))
