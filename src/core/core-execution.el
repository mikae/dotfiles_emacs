;;; package --- Summary
;;; Commentary:
;;; Code:
(defun --and (a b)
  (and a b))

;; Nodes
(cl-defun en/create (&key name (last '__unnamed__)
                          func (last nil))
  "Create new node node for the execution graph."
  (list 'serika|execution-node
        name
        ()
        ()
        nil
        func))

(defun en/p (node)
  "Return t if node is a execution node."
  (equal 'serika|execution-node (car node)))

(defun en/name (node)
  "Get or set NEW parent of the NODE with INDEX."
  (nth 1 node))

(defun en/link (parent child)
  "Get or set NEW parent of the NODE with INDEX."
  (setf (nth 2 parent) (cons child  (nth 2 parent)))
  (setf (nth 3 child)  (cons parent (nth 3 child))))

(defun en/unlink (parent child)
  "Remove link between PARENT and CHILD."
  (setf (nth 3 child) (cl-remove-if (lambda (node)
                                      (eq node
                                          parent))
                                    (nth 3 child)))
  (setf (nth 2 parent) (cl-remove-if (lambda (node)
                                       (eq node
                                           child))
                                     (nth 2 parent)))
  )

(cl-defun en/parent (node &key index (last nil)
                          &key name  (last nil))
  "Get or set NEW parent of the NODE with INDEX."
  (cond ((numberp index) (nth index (nth 3 node)))
        ((symbolp name)  (and (not (null name))
                              (cl-find-if (lambda (item) (string= name
                                                             (nth 1 item)))
                                          (nth 3 node))))
        (t nil)))

(cl-defun en/child (node &key index (last nil)
                         &key name  (last nil))
  "Get or set NEW child of the NODE with INDEX."
  (cond ((numberp index) (nth index (nth 2 node)))
        ((symbolp name)  (and (not (null name))
                              (cl-find-if (lambda (item) (string= name
                                                             (nth 1 item)))
                                          (nth 2 node))))
        (t nil)))

(defun en/child-count (node)
  "Return child count of NODE."
  (length (nth 2 node)))

(defun en/parent-count (node)
  "Return parent count of NODE."
  (length (nth 3 node)))

(defun en/executedp (node)
  "Return t if NODE is executed"
  (nth 4 node))

(defun en/executablep (node)
  "Return t if NODE is executed"
  (and (not (en/executedp node))
       (if (> (length (nth 3 node)) 0)
           (cl-reduce #'--and (mapcar #'en/executedp (nth 3 node)))
         t)))

(defun en/execute (node)
  "Execute node."
  (when (en/executablep node)
    (when (nth 5 node)
      (funcall (nth 5 node)))
    (setf (nth 4 node) t)
    (mapcar #'en/execute (nth 2 node))))

;; Graph
(defun eg/create ()
  "Create execution graph."
  (let ((graph (list 'serika|execution-graph
                     ()
                     (make-hash-table :test 'equal))))
    graph))

(defun eg/p (graph)
  "Return t if GRAPH is execution graph."
  (and (eq (car graph)
           'serika|execution-graph)))

(defun eg/get (graph path)
  "Return node in graph by path."
  (let* ((path-elements (split-string path))
         (node (gethash (car path-elements) (nth 2 graph))))
    (dolist (pe (cdr path-elements))
      (setq node (en/child node :name (make-symbol pe))))
    node))

(cl-defun eg/add (graph &key name (last '__unnamed__)
                        &key parents (last nil)
                        &key func (last nil)
                        &key node (last nil))
  "Create new node with target PARENTS, FUNC and NAME.
PARENTS is list of paths to parent nodes."
  (let ((--node (or node
                    (en/create :name name
                               :func func))))
    (if (not parents)
        (setf (nth 1 graph) (cons --node (nth 1 graph)))
      (dolist (--parent parents)
        (let ((--parent-node (eg/get graph --parent)))
          (en/link --parent-node --node))))
    (puthash (symbol-name (en/name --node)) --node (nth 2 graph))))

(cl-defun eg/event-node (graph
                         &key name (last '__event_node__)
                         &key func (last nil))
  (lexical-let ((node (en/create :name name
                         :func func)))
    (eg/add graph
            :parents nil
            :node node)
    (lambda ()
      (en/execute node))))

(defun eg/execute (graph)
  "Execute "
  (dolist (elem (nth 1 graph))
    (en/execute elem)))

(provide 'core-execution)
;;; core-execution.el ends here
