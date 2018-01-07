;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Private
(defun --en/find-in-node-list (node node-list
                                    name  name-p
                                    index index-p)
  ""
  (unless (en/p node)
    (error ": NODE should be an execution node: %s" node))

  (unless (or (and name-p       (not index-p))
              (and (not name-p) index-p))
    (error ": NAME or INDEX must be provided"))

  (cond
   ((and name-p
         (symbolp name))
    ;; todo: optimize(?)
    (let ((--node-list node-list)
          (--found))
      (while (and (not --found)
                  --node-list)
        (when (string= name
                       (en/name (car --node-list)))
          (setq --found (car --node-list)))
        (setq --node-list (cdr --node-list)))
      --found))
   ((and index-p
         (numberp index))
    ;; Enumeration is from the end of node-list
    (let* ((--length (length node-list))
           (--index (- --length index 1)))
      (unless (and (>= --index 0)
                   (< --index --length))
        (error ": Incorrent (index length) combination (%d %d)" --index --length))
      (nth --index node-list)))
   (t
    (error ": something strange happened"))))

(defun --en/all-nodes-are-executed-p (node-list)
  ""
  (let ((--result t)
        (--node-list node-list))
    (while (and --result
                --node-list)
      (setq --result
            (and --result
                 (en/executed-p (car node-list))))
      (setq --node-list (cdr --node-list)))
    --result))

(defun --en/execute-list-func (--func)
  ""
  (cond
   ((or (functionp      (car --func))
        (special-form-p (car --func)))
    (eval --func))
   ((macrop (car --func))
    (eval (macroexpand --func)))
   (t (error "--en/execute-list-func: Strange list executable found: %s" --func))))

(defun --en/execute (node)
  ""
  (let ((--func (en/func node)))
    (cond
     ((null --func))
     ((functionp --func)
      (funcall --func))
     ((listp --func)
      (--en/execute-list-func --func))
     (t (error "--en/execute: Strange executable found: %s" --func)))))

;; Public
(cl-defun en/create (&key (name '__unnamed__)
                          (func nil))
  "Create new execution node."
  ;; Check name argument
  (unless (symbolp name)
    (error "en/create: NAME should be a symbol: \"%s\"" name))

  ;; Check func argument
  (unless (or (null func)
              (functionp func)
              (listp func))
    (error "en/create: FUNC is invalid: \"%s\"" func))

  (list 'execution-node
        name
        ()
        ()
        nil
        func))

(defun en/p (node)
  ""
  (and node
       (listp node)
       (eq (length node)
           6)
       (eq (car node)
           'execution-node)))

(defun en/name (node)
  ""
  (unless (en/p node)
    (error "en/name: NODE should be an execution node: %s" node))

  (nth 1 node))

(defun en/children (node)
  ""
  (unless (en/p node)
    (error "en/children: NODE should be an execution node: %s" node))

  (nth 2 node))

(defun en/parents (node)
  ""
  (unless (en/p node)
    (error "en/parents: NODE should be an execution node: %s" node))

  (nth 3 node))

(defun en/executed-p (node)
  ""
  (unless (en/p node)
    (error "en/executed: NODE should be an execution node: %s" node))

  (nth 4 node))

(defun en/func (node &optional value)
  ""
  (unless (en/p node)
    (error "en/func: NODE should be an execution node: %s" node))

  (if value
      (setf (nth 5 node) value)
    (nth 5 node)))

(defun en/parent-count (node)
  ""
  (unless (en/p node)
    (error "en/parent-count: NODE should be an execution node: %s" node))

  (length (en/parents node)))

(defun en/children-count (node)
  ""
  (unless (en/p node)
    (error "en/children-count: NODE should be an execution node: %s" node))

  (length (en/children node)))

(cl-defun en/parent (node &key
                          (name  nil name-p)
                          (index 0   index-p))
  ""
  (--en/find-in-node-list node  (en/parents node)
                          name  name-p
                          index index-p))

(cl-defun en/child (node &key
                         (name  nil name-p)
                         (index 0   index-p))
  ""
  (--en/find-in-node-list node  (en/children node)
                          name  name-p
                          index index-p))

(defun en/link (parent child)
  ""
  (unless (en/p parent)
    (error "en/link: PARENT must be an execution node: %s" parent))

  (unless (en/p child)
    (error "en/link: CHILD must be an execution node: %s"  child))

  (when (or (cl-member parent (en/parents  child))
            (cl-member child  (en/children parent)))
    (error "en/link: PARENT and CHILD already linked"))

  (push parent (nth 3 child))
  (push child  (nth 2 parent)))

(defun en/executable-p (node)
  ""
  (unless (en/p node)
    (error "en/executeable-p: NODE must be an execution node: %s" node))

  (and (--en/all-nodes-are-executed-p (en/parents node))
       (not (en/executed-p node))))

(defun en/execute (node)
  ""
  (unless (en/p node)
    (error "en/execute: NODE must be an execution node: %s" node))

  (when (en/executable-p node)
    (setf (nth 4 node) t)
    (--en/execute node)
    (dolist (--child (reverse (en/children node)))
      (en/execute --child))))

(defun en/log (node &optional diff)
  (message (concat (or diff "")
                   "-> "
                   (format "%s: %s" (en/name node) (if (en/executed-p node)
                                                       "t"
                                                     "nil"))))
  (dolist (child (reverse (en/children node)))
    (en/log child (concat (or diff "") "  "))))

(provide '--execution-node)
;;; func-execution.el ends here
