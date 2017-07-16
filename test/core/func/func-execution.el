;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-execution)
(require 'ert)
(require 'cl)

;; Nodes
;; Create
(ert-deftest en|create ()
  (let ((node (en/create)))
    ;; It creates new node
    (should (en/p node))))

;; link
(ert-deftest en|link ()
  (let ((root  (en/create))
        (node (en/create)))
    ;; It links two nodes
    (en/link root node)
    (should (eq (en/parent node
                           :index 0)
                root))
    (should (eq (en/child  root
                           :index 0)
                node))))

(ert-deftest en|link-to-executed ()
  (let ((root  (en/create))
        (node (en/create)))
    ;; If linked to executed parent execute new node after
    (en/execute   root)
    (en/link      root node)
    (should-not (en/executedp node))
    ))

;; unlink
(ert-deftest en|unlink-1 ()
  (let ((root  (en/create))
        (node (en/create)))
    (en/link root node)
    (en/unlink root node)

    ;; Root and node are not parent and node respectively now
    (should-not (en/parent node
                           :index 0))
    (should-not (en/child  root
                           :index 0))

    ;; When 2 unexecuted nodes are unlinked, they shouldn't be executed
    (should-not (en/executedp node))
    (should-not (en/executedp root))
    ))

;; parent
(ert-deftest en|parent-1 ()
  (let ((root-1  (en/create))
        (root-2  (en/create))
        (node (en/create)))
    (en/link root-1 node)
    (should (eq (en/parent node :index 0)
                root-1))
    (should-not (eq (en/parent node :index 0)
                    root-2))
    (should-not (en/parent node :index 10))
    (should-not (en/parent node :name 'hoi))
    (should-not (en/parent node))))

(ert-deftest en|parent-2 ()
  (let ((root-1  (en/create :name 'a))
        (root-2  (en/create :name 'b))
        (node    (en/create :name 'c)))
    (en/link root-1 node)
    (en/link root-2 node)
    (should (eq (en/parent node :name 'a)
                root-1))
    (should (eq (en/parent node :name 'b)
                root-2))
    (should-not (en/parent node :index 10))
    (should-not (en/parent node :name 'hoi))))

;; child
(ert-deftest en|child-1 ()
  (let ((root    (en/create))
        (node-1 (en/create))
        (node-2 (en/create)))
    (en/link root node-1)
    (should     (eq (en/child root
                              :index 0)
                    node-1))
    (should-not (eq (en/child root
                              :index 0)
                    node-2))
    (should-not (en/child root :index 10))
    (should-not (en/child root :name 'hoi))
    (should-not (en/child root))))

(ert-deftest en|child-2 ()
  (let ((root   (en/create :name 'a))
        (node-1 (en/create :name 'b))
        (node-2 (en/create :name 'c)))
    (en/link root node-1)
    (en/link root node-2)
    (should (eq (en/child root :name 'b)
                node-1))
    (should (eq (en/child root :name 'c)
                node-2))))

;; child-count
(ert-deftest en|child-count ()
  (let ((root    (en/create))
        (node-1 (en/create))
        (node-2 (en/create))
        (node-3 (en/create)))
    (should (= (en/child-count root)
               0))

    (en/link root node-1)
    (should (= (en/child-count root)
               1))

    (en/link root node-1)
    (should (= (en/child-count root)
               2))

    (en/link root node-1)
    (should (= (en/child-count root)
               3))))

;; parent-count
(ert-deftest en|parent-count ()
  (let ((node  (en/create))
        (root-1 (en/create))
        (root-2 (en/create))
        (root-3 (en/create)))
    (should (= (en/parent-count node)
               0))

    (en/link root-1 node)
    (should (= (en/parent-count node)
               1))

    (en/link root-2 node)
    (should (= (en/parent-count node)
               2))

    (en/link root-3 node)
    (should (= (en/parent-count node)
               3))))

;; executedp
(ert-deftest en|executedp-1 ()
  (let ((node (en/create)))
    ;; New node is not executed
    (should-not (en/executedp node))))

(ert-deftest en|executedp-2 ()
  (let ((root-1  (en/create))
        (root-2  (en/create))
        (node-1 (en/create)))
    ;; New nodes are not executed yet
    (should-not (en/executedp root-1))
    (should-not (en/executedp root-2))
    (should-not (en/executedp node-1))

    ;; Creation of links between nodes doesn't make some executed
    (en/link root-1 node-1)
    (en/link root-2 node-1)
    (should-not (en/executedp root-1))
    (should-not (en/executedp root-2))
    (should-not (en/executedp node-1))

    ;; When some parents are not executed, appropriate children must not be executed
    (en/execute root-1)
    (should     (en/executedp root-1))
    (should-not (en/executedp root-2))
    (should-not (en/executedp node-1))

    ;; When all parents are executed, appropriate children must be executed
    (en/execute root-2)
    (should (en/executedp root-1))
    (should (en/executedp root-2))
    (should (en/executedp node-1))))

;; Executablep
(ert-deftest en|executablep-1 ()
  (let ((root    (en/create))
        (node-1 (en/create)))
    ;; New node is executable
    (should (en/executablep root))
    (should (en/executablep node-1))

    ;; Children can't be executed when their parent is not executed
    (en/link root node-1)
    (should (en/executablep root))
    (should-not (en/executablep node-1))
    ))

(ert-deftest en|executablep-2 ()
  (let ((root-1  (en/create))
        (root-2  (en/create))
        (node-1 (en/create)))

    ;; Created node without links is executable
    (should (en/executablep root-1))
    (should (en/executablep root-2))
    (should (en/executablep node-1))

    ;; Children can't be executed when their parent is not executed
    (en/link root-1 node-1)
    (en/link root-2 node-1)
    (should (en/executablep root-1))
    (should (en/executablep root-2))
    (should-not (en/executablep node-1))

    ;; When all nodes are executed, they can't be executed again
    (en/execute root-1)
    (en/execute root-2)
    (should-not (en/executablep root-1))
    (should-not (en/executablep root-2))
    (should-not (en/executablep node-1))
    ))

(ert-deftest en|name ()
  (let ((node (en/create :name 'test)))
    (should (string= (en/name node)
                     'test))))

(ert-deftest en|func ()
  (let* ((func-1 (lambda () ()))
         (func-2 (lambda () ()))
         (node   (en/create :name 'test
                            :func func-1)))
    ;; It gets func
    (should (eq (en/func node)
                func-1))
    (en/func node func-2)
    ;; It sets new func
    (should (eq (en/func node)
                func-2))))

;; ;; execute
(ert-deftest en|execute-1 ()
  (let ((counter 0)
        (node    (en/create :func (lambda ()
                                    (setq counter (1+ counter))))))
    ;; New node is not executed
    (should-not (en/executedp node))

    ;; Callback function should be invoked when the node become executed
    (en/execute node)
    (should (= 1 counter))

    ;; Callback shouldn't be invoked twice
    (en/execute node)
    (should (= 1 counter))

    ;; Finally, node should become executed
    (should (en/executedp node))))

(ert-deftest en|execute-2 ()
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
    ;; No nodes should be executed when they are created
    (should-not (cl-reduce (lambda (a b) (or a b))
                           (mapcar #'en/executedp
                                   --list)))

    (en/link root   node-1)
    (en/link node-1 node-1-1)
    (en/link node-1 node-1-2)
    (en/link root   node-2)
    (en/link node-2 node-2-1)
    (en/link node-2 node-2-2)

    ;; After root's execution other nodes must become executed
    (en/execute root)
    (should (cl-reduce (lambda (a b) (and a b))
                       (mapcar #'en/executedp
                               --list)))))

(ert-deftest en|execute-3 ()
  (let ((root-1 (en/create))
        (root-2 (en/create))
        (root-3 (en/create))
        (node   (en/create)))
    (en/link root-1 node)
    (en/link root-2 node)
    (en/link root-3 node)

    (should-not (en/executedp node))

    (en/execute root-1)
    (should-not (en/executedp node))

    (en/execute root-2)
    (should-not (en/executedp node))

    (en/execute root-3)
    (should     (en/executedp node))))

;; Graph
(ert-deftest eg|create ()
  (let ((graph (eg/create)))
    (should (eg/p graph))))

(ert-deftest eg|add_get ()
  (let ((graph (eg/create))
        (root (en/create :name 'root
                         :func nil)))
    (eg/add graph
            :node root
            :parents nil)
    (should (eq (eg/get graph
                        "root")
                root))
    ))

(ert-deftest eg|dai_mne_im9_hoz9in-1 ()
  (let ((graph (eg/create))
        (node  (en/create)))
    (eg/add graph
            :parents '("a b c" "d e f")
            :node node)
    (should (en/p (eg/get graph "a")))
    (should (en/p (eg/get graph "a b")))
    (should (en/p (eg/get graph "a b c")))

    (should (en/p (eg/get graph "d")))
    (should (en/p (eg/get graph "d e")))
    (should (en/p (eg/get graph "d e f")))
    ))

(ert-deftest ag|add_to_existing ()
  (let ((graph     (eg/create))
        (temp-func (lambda () ())))
    (eg/create-path graph
                    "root nya")
    ;; When add node to existing if new node contains a func, and older doesn't
    ;; set the func of new node to old
    (eg/add graph
            :parents '("root")
            :name 'nya
            :func temp-func)
    (should (eq (en/func (eg/get graph "root nya"))
                temp-func))
    ))

(ert-deftest eg|get_empty ()
  (let ((graph (eg/create)))
    (should-not (eg/get graph "root"))
    (should-not (eg/get graph "root nyan"))))

(ert-deftest eg|create_path ()
  (let ((graph (eg/create)))
    (eg/create-path graph
                    "root nya")
    (should (en/p (eg/get graph "root")))
    (should (en/p (eg/get graph "root nya")))
    (should (eq (en/child (eg/get graph "root") :index 0)
                (eg/get graph "root nya")))
    (should (eq (en/parent (eg/get graph "root nya") :index 0)
      (eg/get graph "root")))))


(ert-deftest eg|execute-1 ()
  (let ((graph (eg/create))
        (counter-1 0)
        (counter-2 0)
        (func-1    (lambda () (setq counter-1 (1+ counter-1))))
        (func-2    (lambda () (setq counter-2 (1+ counter-2)))))
    (eg/add graph
            :parents nil
            :name    'root)
    (eg/add graph
            :parents '("root")
            :func    func-1
            :name    'test-1)
    (eg/add graph
            :parents '("root")
            :func    func-2
            :name    'test-2)
    (eg/execute graph)
    (should (= counter-1
               1))
    (should (= counter-2
               1))))

(ert-deftest eg|execute-2 ()
  (let ((graph (eg/create))
        )
    (eg/add graph
            :parents nil
            :name    'root-1)
    (eg/add graph
            :parents '("root-1")
            :name    'test-1-1)
    (eg/add graph
            :parents '("root-1")
            :name    'test-1-2)

    (eg/add graph
            :parents nil
            :name    'root-2)
    (eg/add graph
            :parents '("root-2")
            :name    'test-2-1)
    (eg/add graph
            :parents '("root-2")
            :name    'test-2-2)

    (eg/add graph
            :parents nil
            :name    'root-3)
    (eg/add graph
            :parents '("root-3")
            :name    'test-3-1)
    (eg/add graph
            :parents '("root-3")
            :name    'test-3-2)

    (eg/add graph
            :parents '("root-1" "root-2" "root-3")
            :name    'multi-child)

    (eg/execute graph)

    (should (en/executedp (eg/get graph "root-1")))
    (should (en/executedp (eg/get graph "root-1 test-1-1")))
    (should (en/executedp (eg/get graph "root-1 test-1-2")))
    (should (en/executedp (eg/get graph "root-2")))
    (should (en/executedp (eg/get graph "root-2 test-2-1")))
    (should (en/executedp (eg/get graph "root-2 test-2-2")))
    (should (en/executedp (eg/get graph "root-3")))
    (should (en/executedp (eg/get graph "root-3 test-3-1")))
    (should (en/executedp (eg/get graph "root-3 test-3-2")))

    (should (en/executedp (eg/get graph "root-1 multi-child")))
    (should (eq (eg/get graph "root-1 multi-child")
                (eg/get graph "root-2 multi-child")))
    (should (eq (eg/get graph "root-1 multi-child")
                (eg/get graph "root-3 multi-child")))
    (should (eq (eg/get graph "root-2 multi-child")
                (eg/get graph "root-3 multi-child")))))

(ert-deftest eg|event-node ()
  (let ((graph   (eg/create))
        (counter 0)
        (func    (lambda () (setq counter (1+ counter))))
        (emit-event))
    (setq emit-event (eg/event-node graph
                                    :name 'nya
                                    :func  func))
    (eg/add graph
            :name 'nyan-1
            :parents '("nya"))

    (eg/add graph
            :name 'nyan-2
            :parents '("nya"))

    (eg/add graph
            :name 'nyan-1-1
            :parents '("nya nyan-1"))

    (funcall emit-event)

    (should (en/executedp (eg/get graph
                                  "nya")))
    (should (en/executedp (eg/get graph
                                  "nya nyan-1")))
    (should (en/executedp (eg/get graph
                                  "nya nyan-2")))
    (should (en/executedp (eg/get graph
                                  "nya nyan-1 nyan-1-1")))))
