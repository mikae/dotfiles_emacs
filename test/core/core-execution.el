;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'core-execution)
(require 'ert)
(require 'cl)

;; Create
(ert-deftest create ()
  (let ((node (serika-c/execution|node/create)))
    ;; It creates new node
    (should (serika-c/execution|node/p node))))

;; link
(ert-deftest link ()
  (let ((root  (serika-c/execution|node/create))
        (node (serika-c/execution|node/create)))
    ;; It links two nodes
    (serika-c/execution|node/link root node)
    (should (eq (serika-c/execution|node/parent node 0)
                                           root))
    (should (eq (serika-c/execution|node/child  root    0)
                                           node))))

(ert-deftest link-to-executed ()
  (let ((root  (serika-c/execution|node/create))
        (node (serika-c/execution|node/create)))
    ;; If linked to executed parent execute new node after
    (serika-c/execution|node/execute   root)
    (serika-c/execution|node/link      root node)
    (should (serika-c/execution|node/executedp node))
    ))

;; unlink
(ert-deftest unlink-1 ()
  (let ((root  (serika-c/execution|node/create))
        (node (serika-c/execution|node/create)))
    (serika-c/execution|node/link root node)
    (serika-c/execution|node/unlink root node)

    ;; Root and node are not parent and node respectively now
    (should-not (serika-c/execution|node/parent    node 0))
    (should-not (serika-c/execution|node/child     root 0))

    ;; When 2 unexecuted nodes are unlinked, they shouldn't be executed
    (should-not (serika-c/execution|node/executedp node))
    (should-not (serika-c/execution|node/executedp root))
    ))

;; parent
(ert-deftest parent ()
  (let ((root-1  (serika-c/execution|node/create))
        (root-2  (serika-c/execution|node/create))
        (node (serika-c/execution|node/create)))
    (serika-c/execution|node/link root-1 node)
    (should (eq (serika-c/execution|node/parent node 0)
                                           root-1))
    (should-not (eq (serika-c/execution|node/parent node 0)
                                               root-2))))

;; child
(ert-deftest child ()
  (let ((root    (serika-c/execution|node/create))
        (node-1 (serika-c/execution|node/create))
        (node-2 (serika-c/execution|node/create)))
    (serika-c/execution|node/link root node-1)
    (should     (eq (serika-c/execution|node/child root 0)
                                               node-1))
    (should-not (eq (serika-c/execution|node/child root 0)
                                               node-2))))

;; child-count
(ert-deftest child-count ()
  (let ((root    (serika-c/execution|node/create))
        (node-1 (serika-c/execution|node/create))
        (node-2 (serika-c/execution|node/create))
        (node-3 (serika-c/execution|node/create)))
    (should (= (serika-c/execution|node/child-count root)
               0))

    (serika-c/execution|node/link root node-1)
    (should (= (serika-c/execution|node/child-count root)
               1))

    (serika-c/execution|node/link root node-1)
    (should (= (serika-c/execution|node/child-count root)
               2))

    (serika-c/execution|node/link root node-1)
    (should (= (serika-c/execution|node/child-count root)
               3))))

;; parent-count
(ert-deftest parent-count ()
  (let ((node  (serika-c/execution|node/create))
        (root-1 (serika-c/execution|node/create))
        (root-2 (serika-c/execution|node/create))
        (root-3 (serika-c/execution|node/create)))
    (should (= (serika-c/execution|node/parent-count node)
               0))

    (serika-c/execution|node/link root-1 node)
    (should (= (serika-c/execution|node/parent-count node)
               1))

    (serika-c/execution|node/link root-2 node)
    (should (= (serika-c/execution|node/parent-count node)
               2))

    (serika-c/execution|node/link root-3 node)
    (should (= (serika-c/execution|node/parent-count node)
               3))))

;; executedp
(ert-deftest executedp-1 ()
  (let ((node (serika-c/execution|node/create)))
    ;; New node is not executed
    (should-not (serika-c/execution|node/executedp node))))

(ert-deftest executedp-2 ()
  (let ((root-1  (serika-c/execution|node/create))
        (root-2  (serika-c/execution|node/create))
        (node-1 (serika-c/execution|node/create)))
    ;; New nodes are not executed yet
    (should-not (serika-c/execution|node/executedp root-1))
    (should-not (serika-c/execution|node/executedp root-2))
    (should-not (serika-c/execution|node/executedp node-1))

    ;; Creation of links between nodes doesn't make some executed
    (serika-c/execution|node/link root-1 node-1)
    (serika-c/execution|node/link root-2 node-1)
    (should-not (serika-c/execution|node/executedp root-1))
    (should-not (serika-c/execution|node/executedp root-2))
    (should-not (serika-c/execution|node/executedp node-1))

    ;; When some parents are not executed, appropriate children must not be executed
    (serika-c/execution|node/execute root-1)
    (should     (serika-c/execution|node/executedp root-1))
    (should-not (serika-c/execution|node/executedp root-2))
    (should-not (serika-c/execution|node/executedp node-1))

    ;; When all parents are executed, appropriate children must be executed
    (serika-c/execution|node/execute root-2)
    (should (serika-c/execution|node/executedp root-1))
    (should (serika-c/execution|node/executedp root-2))
    (should (serika-c/execution|node/executedp node-1))))

;; Executablep
(ert-deftest executablep-1 ()
  (let ((root    (serika-c/execution|node/create))
        (node-1 (serika-c/execution|node/create)))
    ;; New node is executable
    (should (serika-c/execution|node/executablep root))
    (should (serika-c/execution|node/executablep node-1))

    ;; Children can't be executed when their parent is not executed
    (serika-c/execution|node/link root node-1)
    (should (serika-c/execution|node/executablep root))
    (should-not (serika-c/execution|node/executablep node-1))
    ))

(ert-deftest executablep-2 ()
  (let ((root-1  (serika-c/execution|node/create))
        (root-2  (serika-c/execution|node/create))
        (node-1 (serika-c/execution|node/create)))

    ;; Created node without links is executable
    (should (serika-c/execution|node/executablep root-1))
    (should (serika-c/execution|node/executablep root-2))
    (should (serika-c/execution|node/executablep node-1))

    ;; Children can't be executed when their parent is not executed
    (serika-c/execution|node/link root-1 node-1)
    (serika-c/execution|node/link root-2 node-1)
    (should (serika-c/execution|node/executablep root-1))
    (should (serika-c/execution|node/executablep root-2))
    (should-not (serika-c/execution|node/executablep node-1))

    ;; When all nodes are executed, they can't be executed again
    (serika-c/execution|node/execute root-1)
    (serika-c/execution|node/execute root-2)
    (should-not (serika-c/execution|node/executablep root-1))
    (should-not (serika-c/execution|node/executablep root-2))
    (should-not (serika-c/execution|node/executablep node-1))
    ))

;; ;; execute
(ert-deftest execute-1 ()
  (let ((counter 0)
        (node    (serika-c/execution|node/create :func (lambda ()
                                                         (setq counter (1+ counter))))))
    ;; New node is not executed
    (should-not (serika-c/execution|node/executedp node))

    ;; Callback function should be invoked when the node become executed
    (serika-c/execution|node/execute node)
    (should (= 1 counter))

    ;; Callback shouldn't be invoked twice
    (serika-c/execution|node/execute node)
    (should (= 1 counter))

    ;; Finally, node should become executed
    (should (serika-c/execution|node/executedp node))))

(ert-deftest execute-2 ()
  (let* ((root     (serika-c/execution|node/create))
         (node-1   (serika-c/execution|node/create))
         (node-1-1 (serika-c/execution|node/create))
         (node-1-2 (serika-c/execution|node/create))
         (node-2   (serika-c/execution|node/create))
         (node-2-1 (serika-c/execution|node/create))
         (node-2-2 (serika-c/execution|node/create))
         (--list   (list root
                         node-1
                         node-1-1
                         node-1-2
                         node-2
                         node-2-1
                         node-2-2)))
    ;; No nodes should be executed when they are created
    (should-not (cl-reduce (lambda (a b) (or a b))
                           (mapcar #'serika-c/execution|node/executedp
                                   --list)))

    (serika-c/execution|node/link root   node-1)
    (serika-c/execution|node/link node-1 node-1-1)
    (serika-c/execution|node/link node-1 node-1-2)
    (serika-c/execution|node/link root   node-2)
    (serika-c/execution|node/link node-2 node-2-1)
    (serika-c/execution|node/link node-2 node-2-2)

    ;; After root's execution other nodes must become executed
    (serika-c/execution|node/execute root)
    (should (cl-reduce (lambda (a b) (and a b))
                       (mapcar #'serika-c/execution|node/executedp
                               --list)))))
