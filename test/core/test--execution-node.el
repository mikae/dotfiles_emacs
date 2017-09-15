;;; package --- Summary
;;; Commentary:
;;; Code:

(require '--execution-node)
(require 'buttercup)

(describe "Execution node"
  (describe "en/create"
    (it "Returns non-nil list."
      (let ((node (en/create)))
        (expect (null node)
                :to-be nil)))

    (describe "Name argument"
      (it "Can set name argument."
        (en/create :name 'nya))

      (it "Raises error if name argument is not a symbol."
        (should-error (en/create :name "test"))))

    (describe "Function argument"
      (it "Can set function argument."
        (en/create :func (lambda () 1)))

      (it "Passes if function arg is nil."
        (en/create :func nil))

      (it "Passes if function arg is a lambda."
        (en/create :func (lambda () 1)))

      (it "Passes if function arg is a existing function."
        (en/create :func #'functionp))

      (it "Passes if function arg is a macro invocation."
        (en/create :func '(defun something () "" 1)))

      (it "Passes if function arg is a function invocation."
        (en/create :func '(functionp 'test)))))

  (describe "en/p"
    (it "Knows that nil isn't a execution node."
      (expect (en/p nil)
              :to-be nil))

    (it "Knows that execution node is a list."
      (expect (en/p 1)
              :to-be nil))

    (it "Can check if a list is a execution node.."
      (expect (en/p (en/create))
              :to-be t)))

  (describe "en/name"
    (it "Raises an error if arg is not a EN."
      (should-error (en/name 1)))

    (it "Returns name of the node"
      (expect (en/name (en/create :name 'nya))
              :to-be 'nya)))

  (describe "en/children"
    (it "Raises an error if arg is not a EN."
      (should-error (en/children 1)))

    (it "Can get/set children of EN."
      (let ((node-1 (en/create))
            (node-2 (en/create)))
        (en/children node-1
                     (list node-2))
        (expect (en/children node-1)
                :to-equal (list node-2)))))

  (describe "en/parents"
    (it "Raises an error if arg is not a EN."
      (should-error (en/parents 1)))

    (it "Can get/set children of EN."
      (let ((node-1 (en/create))
            (node-2 (en/create)))
        (en/parents node-1
                    (list node-2))
        (expect (en/parents node-1)
                :to-equal (list node-2)))))

  (describe "en/executed"
    (it "Raises error if arg is not a EN."
      (should-error (en/executed 1)))

    (it "Returns nil for new node."
      (expect (en/executed (en/create))
              :to-be nil))

    (it "Returns t for executed node."))

  (describe "en/func"
    (it "Raises an error if arg is nod a EN."
      (should-error (en/func 1)))

    (it "Returns func of the EN."
      (expect (en/func (en/create :func '(defun --test () "" 1)))
              :to-equal '(defun --test () "" 1)))

    (it "Can set func of the EN."
      (let ((node (en/create)))
        (en/func node '(lambda () "" 1))
        (expect (en/func node)
                :to-equal '(lambda () "" 1)))))

  (describe "en/parent"
    (it "Raises an error if arg is not a EN."
      (should-error (en/parent 1 :name 'arst)))

    (it "Passes if only index or name was provided, raises an error otherwise"
      (should-error (en/parent (en/create)))
      (should-error (en/parent (en/create)
                               :name 'arst
                               :index 1))
      (en/parent (en/create)
                 :name 'arst)
      (en/parent (en/create)
                 :index 0))

    (it "Returns parent by index"
      (let ((child  (en/create))
            (parent-1 (en/create))
            (parent-2 (en/create)))
        (en/parents child (list parent-1 parent-2))
        (expect (en/parent child
                           :index 0)
                :to-be parent-1)
        (expect (en/parent child
                           :index 1)
                :to-be parent-2)))

    (it "Returns parent by name"
      (let ((child  (en/create))
            (parent-1 (en/create :name 'a))
            (parent-2 (en/create :name 'b)))
        (en/parents child (list parent-1 parent-2))
        (expect (en/parent child
                           :name 'a)
                :to-be parent-1)
        (expect (en/parent child
                           :name 'b)
                :to-be parent-2))))

  (describe "en/child"
    (it "Raises an error if arg is not a EN."
      (should-error (en/child 1 :name 'arst)))

    (it "Passes only if index or name was provided, raises an error otherwise"
      (should-error (en/child (en/create)))
      (should-error (en/child (en/create)
                              :name 'arst
                              :index 1))
      (en/child (en/create)
                :name 'arst)
      (en/child (en/create)
                :index 0))

    (it "Returns child by index"
      (let ((parent  (en/create))
            (child-1 (en/create))
            (child-2 (en/create)))
        (en/children parent (list child-1 child-2))
        (expect (en/child parent
                          :index 0)
                :to-be child-1)
        (expect (en/child parent
                          :index 1)
                :to-be child-2)))

    (it "Returns child by name"
      (let ((parent  (en/create))
            (child-1 (en/create :name 'a))
            (child-2 (en/create :name 'b)))
        (en/children parent (list child-1 child-2))
        (expect (en/child parent
                          :name 'a)
                :to-be child-1)
        (expect (en/child parent
                          :name 'b)
                :to-be child-2))))

  (describe "en/link"
    (it "Raises an error if any arg is not a EN."
      (should-error (en/link 1           2))
      (should-error (en/link (en/create) 2))
      (should-error (en/link 1           (en/create)))
      (en/link      (en/create)          (en/create))))
  )

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
