;;; package --- Summary
;;; Commentary:
;;; Code:
(defun --and (a b)
  (and a b))

(cl-defun serika-c/execution|node/create (&key name (last "") func (last nil))
  "Create new node node for the execution graph."
  (list 'serika|execution-node
        name
        ()
        ()
        nil
        func))

(defun serika-c/execution|node/p (node)
  "Return t if node is a execution node."
  (equal 'serika|execution-node (car node)))

(defun serika-c/execution|node/link (parent child)
  "Get or set NEW parent of the NODE with INDEX."
  (setf (nth 2 parent) (cons child  (nth 2 parent)))
  (setf (nth 3 child)  (cons parent (nth 3 child)))
  (when (serika-c/execution|node/executedp parent)
    (serika-c/execution|node/execute child)))

(defun serika-c/execution|node/unlink (parent child)
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

(defun serika-c/execution|node/parent (node index)
  "Get or set NEW parent of the NODE with INDEX."
  (nth index (nth 3 node)))

(defun serika-c/execution|node/child (node index)
  "Get or set NEW child of the NODE with INDEX."
  (nth index (nth 2 node)))

(defun serika-c/execution|node/child-count (node)
  "Return child count of NODE."
  (length (nth 2 node)))

(defun serika-c/execution|node/parent-count (node)
  "Return parent count of NODE."
  (length (nth 3 node)))

(defun serika-c/execution|node/executedp (node)
  "Return t if NODE is executed"
  (nth 4 node))

(defun serika-c/execution|node/executablep (node)
  "Return t if NODE is executed"
  (and (not (serika-c/execution|node/executedp node))
       (if (> (length (nth 3 node)) 0)
           (cl-reduce #'--and (mapcar #'serika-c/execution|node/executedp (nth 3 node)))
         t)))

(defun serika-c/execution|node/execute (node)
  "Execute node."
  (when (serika-c/execution|node/executablep node)
    (when (nth 5 node)
      (funcall (nth 5 node)))
    (setf (nth 4 node) t)
    (mapcar #'serika-c/execution|node/execute (nth 2 node))))

(provide 'core-execution)
;;; core-execution.el ends here
