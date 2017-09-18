(require 'buttercup)
(require '--execution-graph)

(describe "Execution graph"
  (describe "--eg/find-child-by-path"
    (it "Finds it"
      (let ((node-1     (en/create :name 'node-1))
            (node-1-1   (en/create :name 'node-1-1))
            (node-1-1-1 (en/create :name 'node-1-1-1)))
        (en/link node-1   node-1-1)
        (en/link node-1-1 node-1-1-1)
        (expect (--eg/find-child-by-path node-1 '("node-1-1" "node-1-1-1"))
                :to-be node-1-1-1))
      ))

  (describe "eg/create"
    (it "Returns non-nil list"
      (expect (eg/create)
              :not :to-be nil))

    (it "Returns execution graph"
      (expect (eg/p (eg/create))
              :to-be t)))

  (describe "eg/p"
    (it "Knows that only lists, that returned by `eg/create' method are valid EG"
      (expect (eg/p (eg/create))
              :to-be t)))

  (describe "eg/roots"
    (it "Raises an error if first arg is not an EG"
      (expect (eg/roots 1)
              :to-throw))

    (it "Returns the roots of EG"
      (let* ((node-1 (en/create))
             (node-2 (en/create))
             (graph (list --execution-graph-identifier
                          `(,node-1
                            ,node-2))))
        (expect (eg/roots graph)
                :to-equal `(,node-1 ,node-2)))))

  (describe "eg/create-path"
    (it "Raises an error if first arg is not an EG"
      (should-error (eg/create-path 1)))

    (it "Raises an error if second arg is not a correct path."
      (expect (eg/create-path (eg/create)
                              1)
              :to-throw))

    (it "Creates path"
      (let ((graph (eg/create)))
        (eg/create-path graph "a b c")
        (expect (list (en/p (eg/get graph "a"))
                      (en/p (eg/get graph "a b"))
                      (en/p (eg/get graph "a b c")))
                :to-equal '(t t t)))))

  (describe "eg/get"
    (it "Raises an error if first arg is not an EG"
      (should-error (eg/get 1 2)))

    (it "Raises an error if second arg is not a path string"
      (should-error (eg/get (eg/create) 1))
      (should-error (eg/get (eg/create) "")))

    (it "Returns node by path")
    )

  (describe "eg/add"
    (it "Raises an error if first arg is not an EG"
      (should-error (eg/add 1
                            :node (en/create))))

    (it "Raises a signal if (name func) AND node was provided"
      (expect (eg/add (eg/create)
                      :name 'a
                      :func (lambda () "" 1)
                      :node (en/create))
              :to-throw))

    (it "When no actual parents, add node to the roots"
      (let ((graph (eg/create))
            (node (en/create :name 'a)))
        (eg/add graph
                :node node)
        (expect (eg/get graph "a")
                :to-be node)))

    (it "Doesn't add the same node to the roots twicely"
      (let ((graph (eg/create))
            (node (en/create :name 'a)))
        (eg/add graph
                :node node
                :parents '("" ""))
        (expect (length (eg/roots graph))
                :to-be 1)))

    (it "When a correct(!) parent exists, add to it"
      (let ((graph (eg/create))
            (node-1 (en/create :name 'node-1))
            (node-2 (en/create :name 'node-2)))
        (eg/add graph
                :node node-1)
        (eg/add graph
                :parents '("node-1")
                :node    node-2)
        (expect (eg/get graph "node-1 node-2")
                :to-be node-2)))

    (it "When a correct(!) parent exists, add to it. Longer version."
      (let ((graph  (eg/create))
            (node-a (en/create :name 'a))
            (node-r (en/create :name 'r))
            (node-s (en/create :name 's))
            (node-t (en/create :name 't))
            (node-d (en/create :name 'd)))
        (eg/add graph
                :node node-a)
        (eg/add graph
                :node    node-r
                :parents '("a"))
        (eg/add graph
                :node    node-s
                :parents '("a r"))
        (eg/add graph
                :node    node-t
                :parents '("a r s"))
        (eg/add graph
                :node    node-d
                :parents '("a r s t"))
        (expect (eg/get graph "a r s t d")
                :to-be node-d)))

    (it "When some parts of path, except root, doesn't exist, create it."
      (let ((graph (eg/create))
            (node-a  (en/create :name 'a))
            (node-t  (en/create :name 't)))
        (eg/add graph
                :node node-a)
        (eg/add graph
                :node node-t
                :parents '("a r s"))
        (expect (eg/get graph "a r s t")
                :to-be node-t)))

    (it "When whole path doesn't exist, create it"
      (let ((graph (eg/create))
            (node-t  (en/create :name 't)))
        (eg/add graph
                :node node-t
                :parents '("a r s"
                           "q w f"))
        (expect (en/p (eg/get graph "a"))
                :to-be t)
        (expect (en/p (eg/get graph "a r"))
                :to-be t)
        (expect (en/p (eg/get graph "a r s"))
                :to-be t)
        (expect (en/p (eg/get graph "q"))
                :to-be t)
        (expect (en/p (eg/get graph "q w"))
                :to-be t)
        (expect (en/p (eg/get graph "q w f"))
                :to-be t)
        (expect (eg/get graph "a r s t")
                :to-be node-t)
        (expect (eg/get graph "q w f t")
                :to-be node-t)
        (expect (length (eg/roots graph))
                :to-be 2)))

    (it "When at the path another node already exists, replace its function"
      (let* ((--sync "")
             (graph  (eg/create))
             (func-1 (lambda () (setq --sync (concat --sync "1"))))
             (func-2 (lambda () (setq --sync (concat --sync "2"))))
             (node-1 (en/create :name 's
                                :func func-1))
             (node-2 (en/create :name 's
                                :func func-2)))
        (eg/add graph
                :node node-1
                :parents '("a r"))
        (eg/add graph
                :node node-2
                :parents '("a r"))
        (eg/execute graph)
        (expect (en/func (eg/get graph "a r s"))
                :to-be func-2)
        (expect (en/executed-p node-1)
                :to-be t)
        (expect (en/executed-p node-2)
                :to-be nil)
        (expect --sync
                :to-equal "2")))

    (it "When all parents, to whom node will be added, are executed, execute this node"
      (let ((graph  (eg/create))
            (node-1 (en/create :name 'node-1))
            (node-2 (en/create :name 'node-2))
            (node-3 (en/create :name 'node-3))
            (child  (en/create :name 'child)))
        (eg/add graph
                :node node-1)
        (eg/add graph
                :node node-2)
        (eg/add graph
                :node node-3)

        (eg/execute graph)

        (eg/add graph
                :node child
                :parents '("node-1"
                           "node-2"
                           "node-3"))
        (expect (en/executed-p child)
                :to-be t)
        ))

    )

  (describe "eg/execute"
    (it "Raises an error if first arg is not an EG"
      (expect (eg/execute 1)
              :to-throw))

    (it "Executes execution graph in correct order: first root, second root"
      (let ((--sync "")
            (node-1 (en/create :func (lambda () (setq --sync (concat --sync "1")))))
            (node-2 (en/create :func (lambda () (setq --sync (concat --sync "2")))))
            (node-3 (en/create :func (lambda () (setq --sync (concat --sync "3")))))
            (graph  (eg/create)))
        (eg/add graph :node node-1)
        (eg/add graph :node node-2)
        (eg/add graph :node node-3)
        (eg/execute graph)
        (expect --sync
                :to-equal "123")
        ))
    )
  )

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
