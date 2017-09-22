;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'buttercup)

(describe "func/keymap/{create,define,define-global}"
  (defmacro --invoke (func keymap use-keymap &rest args)
    (if use-keymap
        `(,func ,keymap ,@args)
      `(,func ,@args)))

  (cl-defmacro define-keymap-tests (func keymap &optional pre post use-keymap)
    `(progn
       nil
       (defun --set-keymap ()
         (setq ,keymap (make-sparse-keymap)))

       (defun --unset-keymap ()
         (makunbound ',keymap))

       (before-each
        (funcall (symbol-function (or ',pre '--set-keymap))))

       (after-each
        (funcall (symbol-function (or ',post '--unset-keymap))))

       (when ,use-keymap
         (it "When arg is not a keymap, raises an error"
           (expect (,func 1)
                   :to-throw)))

       (it "Binds by symbol"
         (--invoke ,func ,keymap ,use-keymap
                   "a" 'arst)
         (expect (lookup-key ,keymap "a")
                 :to-be 'arst))

       (it "Binds by function"
         (--invoke ,func ,keymap ,use-keymap
                   "a" #'arst)
         (expect (lookup-key ,keymap "a")
                 :to-be 'arst))

       (it "Binds by lambda"
         (--invoke ,func ,keymap ,use-keymap
                   "a" (lambda ()
                         (interactive)
                         3))
         (expect (funcall (lookup-key ,keymap "a"))
                 :to-be 3))

       (it "Binds by macro invocation"
         (defmacro --macro ()
           3)

         (--invoke ,func ,keymap ,use-keymap
                   "a" (--macro))
         (expect (funcall (lookup-key ,keymap "a"))
                 :to-be 3)

         (fmakunbound '--macro))

       (it "Binds by function invocation"
         (setq test-arg 3)
         (defun --func (arg)
           arg)

         (--invoke ,func ,keymap ,use-keymap
                   "a" (--func test-arg))
         (expect (funcall (lookup-key ,keymap "a"))
                 :to-be test-arg)

         (fmakunbound '--func))

       (makunbound '--set-keymap)
       (makunbound '--unset-keymap)
       )
    )

  (describe "func/keymap/create"
    (define-keymap-tests func/keymap/create test-keymap ignore --unset-keymap :use-keymap))

  (describe "func/keymap/define"
    (define-keymap-tests func/keymap/define test-keymap --set-keymap --unset-keymap :use-keymap))

  (describe "func/keymap/define-global"
    (define-keymap-tests func/keymap/define-global global-map --set-keymap ignore))

  (fmakunbound 'test-func))

(describe "func/keymap/save"
  (before-each
   (setq test-keymap-1 (make-sparse-keymap))
   (setq test-keymap-2 (make-sparse-keymap)))

  (after-each
   (makunbound 'test-keymap-1)
   (makunbound 'test-keymap-2)
   (makunbound '--func-saved-keymap-test-keymap-1)
   (makunbound '--func-saved-keymap-test-keymap-2))

  (it "Raises an error, if arg is not a keaymap"
    (setq a 1)
    (expect (func/keymap/save a)
            :to-throw))

  (it "Saves keymap"
    (func/keymap/save test-keymap-1)
    (expect test-keymap-1
            :to-be --func-saved-keymap-test-keymap-1))

  (it "Saves multiple keymap"
    (func/keymap/save test-keymap-1
                      test-keymap-2)
    (expect test-keymap-1
            :to-be --func-saved-keymap-test-keymap-1)
    (expect test-keymap-2
            :to-be --func-saved-keymap-test-keymap-2)
    )

  (it "Doesn't save keymap, if some arg is not keymap"
    (setq a 1)
    (expect (func/keymap/save test-keymap-1
                              a)
            :to-throw)
    (expect (boundp '--func-saved-keymap-test-keymap-1)
            :to-be nil)))

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
