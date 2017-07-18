;;; package --- Summary -*- lexical-binding: t -*-
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
  "Get name of the NODE."
  (nth 1 node))

(defun en/children (node &optional value)
  "Get/set children of the NODE."
  (when value
    (setf (nth 2 node) value))
  (nth 2 node))

(defun en/parents (node &optional value)
  "Get/set children of the NODE."
  (when value
    (setf (nth 3 node) value))
  (nth 3 node))

(defun en/executed (node &optional value)
  "Return t if NODE is executed"
  (when value
    (setf (nth 4 node) value))
  (nth 4 node))

(defun en/func (node &optional value)
  "Get func of the node"
  (when value
    (setf (nth 5 node) value))
  (nth 5 node))

(defun en/link (parent child)
  "Get or set NEW parent of the NODE with INDEX."
  (en/children parent (cons child  (en/children parent)))
  (en/parents  child  (cons parent (en/parents  child))))

(defun en/unlink (parent child)
  "Remove link between PARENT and CHILD."
  (setf (nth 3 child) (cl-remove-if (lambda (node)
                                      (eq node
                                          parent))
                                    (en/parents child)))
  (setf (nth 2 parent) (cl-remove-if (lambda (node)
                                       (eq node
                                           child))
                                     (en/children parent)))
  )

(cl-defun en/parent (node &key index (last nil)
                          &key name  (last nil))
  "Get or set NEW parent of the NODE with INDEX."
  (cond ((numberp index) (nth index (en/parents node)))
        ((symbolp name)  (and (not (null name))
                              (cl-find-if (lambda (item) (string= name
                                                             (en/name item)))
                                          (en/parents node))))
        (t nil)))

(cl-defun en/child (node &key index (last nil)
                         &key name  (last nil))
  "Get or set NEW child of the NODE with INDEX."
  (cond ((numberp index) (nth index (en/children node)))
        ((symbolp name)  (and (not (null name))
                              (cl-find-if (lambda (item) (string= name
                                                             (en/name item)))
                                          (en/children node))))
        (t nil)))

(defun en/child-count (node)
  "Return child count of NODE."
  (length (en/children node)))

(defun en/parent-count (node)
  "Return parent count of NODE."
  (length (en/parents node)))

(defun en/executablep (node)
  "Return t if NODE is executed"
  (and (not (en/executed node))
       (if (> (en/parent-count node) 0)
           (cl-reduce #'--and (mapcar #'en/executed (en/parents node)))
         t)))

(defun en/execute (node)
  "Execute node."
  (when (en/executablep node)
    (when (en/func node)
      (funcall (en/func node)))
    (en/executed node t)
    (mapcar #'en/execute (reverse (en/children node)))))

(defun en/log (node &optional diff)
  (message (concat (or diff "")
                   "-> "
                   (format "%s: %s" (en/name node) (if (en/executed node)
																											 "t"
																										 "nil"))))
  (dolist (child (reverse (en/children node)))
    (en/log child (concat (or diff "") "  "))))

;; Graph
(defun eg/create ()
  "Create execution graph."
  (let ((graph (list 'serika|execution-graph
                     ())))
    graph))

(defun eg/roots (graph &optional value)
  "Get roots of GRAPH."
  (when value
    (setf (nth 1 graph) value))
  (nth 1 graph))

(defun eg/p (graph)
  "Return t if GRAPH is execution graph."
  (and (eq (car graph)
           'serika|execution-graph)))

(defun eg/create-path (graph path)
  "Create nodes with path. Doesn't override."
  (let* ((path-elements (split-string path))
         (node (eg/get graph (car path-elements)))
         (counter 1))
    (unless node
      (setq node (en/create :name (make-symbol (car path-elements))))
      (eg/add graph
              :parents nil
              :node node)
      )
    (dolist (pe (cdr path-elements))
      (setq node (en/child node
                           :name (make-symbol pe)))
      (unless node
        (setq node (en/create :name (make-symbol pe)))
        (eg/add graph
                :parents (list (mapconcat 'identity (cl-subseq path-elements 0 counter) " "))
                :node node)
        )
      (setq counter (1+ counter))
      )))

(defun eg/get (graph path)
  "Return node in graph by path."
  (let* ((path-elements (split-string path))
         (node))
    (dolist (root (eg/roots graph))
      (when (string= (en/name root)
                     (car path-elements))
        (setq node root)))
    (when node
      (dolist (pe (cdr path-elements))
        (setq node (en/child node :name (make-symbol pe)))))
    node))

(cl-defun eg/add (graph &key name (last '__unnamed__)
                        &key parents (last nil)
                        &key func (last nil)
                        &key node (last nil))
  "Create new node with target PARENTS, FUNC and NAME.
PARENTS is list of paths to parent nodes."
  (let* ((--old-node (eg/get graph
                             (concat (or (car parents) "") " " (symbol-name name))))
         (--node (or node
                     (en/create :name name
                                :func func))))
    (if --old-node
        (en/func --old-node (en/func --node))
      (progn
        (if (not parents)
            (eg/roots graph (cons --node (eg/roots graph)))
          (dolist (--parent parents)
            (let ((--parent-node (eg/get graph --parent)))
              (unless --parent-node
                (eg/create-path graph
                                --parent)
                (setq --parent-node (eg/get graph --parent)))
              (en/link --parent-node --node))))))))

(cl-defun eg/event-node (graph
                         &key name (last '__event_node__))
  (let ((event-node (en/create :name (concat "event_node__"
                                                     (symbol-name name))))
                (root       (en/create :name name)))
    (en/link event-node root)
    (eg/add graph
            :parents nil
            :node root)
    (lambda ()
      (en/execute event-node))))

(defun eg/execute (graph)
  "Execute "
  (mapcar #'en/execute (reverse (eg/roots graph))))

(defun eg/log (graph)
  (mapcar #'en/log (reverse (eg/roots graph)))
  (switch-to-buffer "*Messages*"))

(provide 'func-execution)
;;; func-execution.el ends here
