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
    (error "en/parent: NAME or INDEX must be provided"))

  (cond
   ((and name-p
         (symbolp name))
    ;; todo: optimize(?)
    (cl-find-if (lambda (item)
                  (string= name
                           (en/name item)))
                node-list))
   ((and index-p
         (numberp index))
    (nth index node-list))
   (t
    (error "en/parent: something strange happened"))))

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
              (and (listp func)
                   (or (macrop    (car func))
                       (functionp (car func)))))
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
  (if (en/p node)
      (nth 1 node)
    (error "en/name: NODE should be an execution node: %s" node)))

(defun en/children (node &optional value)
  ""
  (if (en/p node)
      (if value
          (setf (nth 2 node) value)
        (nth 2 node))
    (error "en/children: NODE should be an execution node: %s" node)))

(defun en/parents (node &optional value)
  ""
  (if (en/p node)
      (if value
          (setf (nth 3 node) value)
        (nth 3 node))
    (error "en/parents: NODE should be an execution node: %s" node)))

(defun en/executed (node)
  ""
  (if (en/p node)
      (nth 4 node)
    (error "en/executed: NODE should be an execution node: %s" node)))

(defun en/func (node &optional value)
  ""
  (if (en/p node)
      (if value
          (setf (nth 5 node) value)
        (nth 5 node))
    (error "en/func: NODE should be an execution node: %s" node)))

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
  )

(provide '--execution-node)
;;; func-execution.el ends here
