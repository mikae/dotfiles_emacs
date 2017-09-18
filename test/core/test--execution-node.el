;;; package --- Summary
;;; Commentary:
;;; Code:

(require '--execution-node)
(require 'buttercup)
(require 'cl-lib)

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
        (en/create :func '(functionp 'test)))

      (it "Passes if function arg is just a list"
        (en/create :func '(abra test)))))

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

    (it "Can get children of EN."
      (let ((child-1 (en/create))
            (child-2 (en/create))
            (parent (en/create)))
        (en/link parent child-1)
        (expect (en/children parent)
                :to-equal (list child-1))

        (en/link parent child-2)
        (expect (en/children parent)
                :to-equal (list child-1 child-2)))))

  (describe "en/parents"
    (it "Raises an error if arg is not a EN."
      (should-error (en/parents 1)))

    (it "Can get parents of EN."
      (let ((child    (en/create))
            (parent-1 (en/create))
            (parent-2 (en/create)))
        (en/link parent-1 child)
        (expect (en/parents child)
                :to-equal (list parent-1))

        (en/link parent-2 child)
        (expect (en/parents child)
                :to-equal (list parent-1 parent-2)))))

  (describe "en/executed"
    (it "Raises error if arg is not a EN."
      (should-error (en/executed-p 1)))

    (it "Returns nil for new node."
      (expect (en/executed-p (en/create))
              :to-be nil))

    (it "Returns t for executed node."
      (let ((node (en/create)))
        (en/execute node)
        (expect (en/executed-p node)
                :to-be t))))

  (describe "en/func"
    (it "Raises an error if arg is not a EN."
      (should-error (en/func 1)))

    (it "Returns func of the EN."
      (expect (en/func (en/create :func '(defun --test () "" 1)))
              :to-equal '(defun --test () "" 1)))

    (it "Can set func of the EN."
      (let ((node (en/create)))
        (en/func node '(lambda () "" 1))
        (expect (en/func node)
                :to-equal '(lambda () "" 1)))))

  (describe "en/parent-count"
    (it "Raises an error if arg is not a EN."
      (should-error (en/parent-count 1)))

    (it "Returns count of parents of EN."
      (let ((child (en/create))
            (parent-1 (en/create))
            (parent-2 (en/create)))
        (expect (en/parent-count child)
                :to-be 0)

        (en/link parent-1 child)
        (expect (en/parent-count child)
                :to-be 1)

        (en/link parent-2 child)
        (expect (en/parent-count child)
                :to-be 2))))

  (describe "en/children-count"
    (it "Raises an error if arg is not a EN."
      (should-error (en/children-count 1)))

    (it "Returns count of children of EN."
      (let ((parent (en/create))
            (child-1 (en/create))
            (child-2 (en/create)))
        (expect (en/children-count parent)
                :to-be 0)

        (en/link parent child-1)
        (expect (en/children-count parent)
                :to-be 1)

        (en/link parent child-2)
        (expect (en/children-count parent)
                :to-be 2))))

  (describe "en/parent"
    (it "Raises an error if arg is not a EN."
      (should-error (en/parent 1 :name 'arst)))

    (it "Passes if only index or name was provided, raises an error otherwise"
      (should-error (en/parent (en/create)))
      (should-error (en/parent (en/create)
                               :name 'arst
                               :index 1))
      (let ((parent (en/create :name 'arst))
            (child  (en/create)))
        (en/link parent child)
        (en/parent child
                   :name 'arst)
        (en/parent child
                   :index 0)))

    (it "Returns parent by index"
      (let ((child  (en/create))
            (parent-1 (en/create))
            (parent-2 (en/create)))
        (en/link parent-1 child)
        (en/link parent-2 child)

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
        (en/link parent-1 child)
        (en/link parent-2 child)

        (expect (en/parent child
                           :name 'a)
                :to-be parent-1)
        (expect (en/parent child
                           :name 'b)
                :to-be parent-2)))

    (it "Raises error if index is incorrect"
      (let ((child (en/create)))
        (should-error (en/parent child :index -1))
        (should-error (en/parent child :index 100)))))

  (describe "en/child"
    (it "Raises an error if arg is not a EN."
      (should-error (en/child 1 :name 'arst)))

    (it "Passes only if index or name was provided, raises an error otherwise"
      (should-error (en/child (en/create)))
      (should-error (en/child (en/create)
                              :name 'arst
                              :index 1))
      (let ((parent (en/create))
            (child  (en/create :name 'arst)))
        (en/link parent child)
        (en/child parent
                  :name 'arst)
        (en/child parent
                  :index 0)))

    (it "Returns child by index"
      (let ((parent  (en/create))
            (child-1 (en/create))
            (child-2 (en/create)))
        (en/link parent child-1)
        (en/link parent child-2)
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
        (en/link parent child-1)
        (en/link parent child-2)
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
      (en/link      (en/create)          (en/create)))

    (it "Links two nodes"
      (let ((child  (en/create))
            (parent (en/create)))
        (en/link parent child)
        (expect (en/parent child
                           :index 0)
                :to-be parent)
        (expect (en/child parent
                          :index 0)
                :to-be child)))

    (it "Raises an error if an attempt to link twicely."
      (let ((child (en/create))
            (parent (en/create)))
        (en/link parent child)
        (should-error (en/link parent child))))

    (it "When parent is a an executed node, don't execute child"
      (let ((parent (en/create))
            (child  (en/create)))
        (en/execute parent)
        (en/link parent child)
        (expect (en/executed-p child)
                :to-be nil))))

  (describe "en/executable-p"
    (it "Raises an error if arg is not a EN."
      (should-error (en/executable-p 1)))

    (it "Newly created nodes are executable"
      (expect (en/executable-p (en/create))))

    (it "Executed nodes are not executeable"
      (let ((node (en/create)))
        (en/execute node)
        (expect (en/executable-p node)
                :to-be nil))
      )

    (it "Nodes, whose parents are not executed, are not executable"
      (let ((parent (en/create))
            (child  (en/create)))
        (en/link parent child)
        (expect (en/executable-p child)
                :to-be nil)))
    )

  (describe "en/execute"
    (it "Raises an error if arg is not a EN."
      (should-error (en/execute 1)))

    (it "Changes state of node to executed"
      (let ((node (en/create)))
        (en/execute node)
        (expect (en/executed-p node)
                :to-be t)))

    (it "Executes a bound function."
      (let* ((--var nil)
             (func (lambda ()
                     (setq --var t)))
             (node (en/create :func func)))
        (en/execute node)
        (expect --var
                :to-be t)))

    (it "Executes a bound function call."
      (setq --test-var nil)
      (defun --test-fun (arg)
        ""
        (setq --test-var arg))

      (let ((node (en/create :func '(--test-fun 5))))
        (en/execute node)
        (expect --test-var
                :to-be 5))

      (makunbound  '--test-var)
      (fmakunbound '--test-fun))

    (it "Executes a bound macro call."
      (let ((node (en/create :func '(defun --test-function () "" 1))))
        (en/execute node)
        (expect (functionp #'--test-function)
                :to-be t))
      (fmakunbound '--test-function))

    (it "Executes all children if they are executable"
      (let ((parent-1 (en/create :name 'parent-1))
            (parent-2 (en/create :name 'parent-2))
            (child-1  (en/create :name 'child-1))
            (child-2  (en/create :name 'child-2)))
        (en/link parent-1 child-1)
        (en/link parent-1 child-2)
        (en/link parent-2 child-2)

        (en/execute parent-1)

        (expect (en/executed-p (en/child parent-1 :index 0))
                :to-be t)
        (expect (en/executed-p (en/child parent-1 :index 1))
                :to-be nil)
        ))

    (it "Executes children in correct order: first, second, ..."
      (let* ((--sync  "")
             (parent  (en/create))
             (child-1 (en/create :func (lambda () (setq --sync (concat --sync "1")))))
             (child-2 (en/create :func (lambda () (setq --sync (concat --sync "2")))))
             (child-3 (en/create :func (lambda () (setq --sync (concat --sync "3")))))
             (child-4 (en/create :func (lambda () (setq --sync (concat --sync "4"))))))
        (en/link parent child-1)
        (en/link parent child-2)
        (en/link parent child-3)
        (en/link parent child-4)
        (en/execute parent)
        (expect --sync
                :to-equal "1234")))

    (it "Doesn't execute twice"
      (let* ((--sync "")
             (node   (en/create :func (lambda () (setq --sync (concat --sync "A"))))))
        (en/execute node)
        (en/execute node)
        (expect --sync
                :to-equal "A")))

    (it "Execution of unknown entity raises an error"
      (let ((node (en/create :func '(a r s))))
        (expect (en/execute node)
                :to-throw)))

    (it "Integrity test 1"
      (let ((parent-1 (en/create))
            (parent-2 (en/create))
            (child  (en/create)))
        (en/link parent-1 child)
        (en/link parent-2 child)
        (en/execute parent-1)
        (expect (en/executed-p parent-1)
                :to-be t)
        (expect (en/executed-p child)
                :to-be nil)

        (en/execute parent-2)
        (expect (en/executed-p parent-2)
                :to-be t)
        (expect (en/executed-p child)
                :to-be t)))

    (it "Integrity test 2"
      (let* ((root     (en/create))
             (node-1   (en/create))
             (node-1-1 (en/create))
             (node-1-2 (en/create))
             (node-2   (en/create))
             (node-2-1 (en/create))
             (node-2-2 (en/create))
             (--list   (list root
                             node-1
                             node-1-1
                             node-1-2
                             node-2
                             node-2-1
                             node-2-2)))
        (en/link root   node-1)
        (en/link node-1 node-1-1)
        (en/link node-1 node-1-2)
        (en/link root   node-2)
        (en/link node-2 node-2-1)
        (en/link node-2 node-2-2)

        ;; After root's execution other nodes must become executed
        (en/execute root)
        (expect (cl-reduce (lambda (a b) (and a b))
                           (mapcar #'en/executed-p
                                   --list))
                :to-be t)))))

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
