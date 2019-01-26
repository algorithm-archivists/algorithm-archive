;;;; Tree traversal in Common Lisp

(defstruct node data children)

(defun dfs-recursive (node)
  (when (not (eql (node-data node) Nil))
    (print (node-data node)))
  (loop for child in (node-children node)
    do (dfs-recursive child)))

(defun dfs-recursive-postorder (node)
  (loop for child in (node-children node)
    do (dfs-recursive-postorder child))
  (when (not (eql (node-data node) Nil))
    (print (node-data node))))

(defun dfs-recursive-inorder-btree (node)
  (cond
    (
      (eql (length (node-children node)) 2)
      (dfs-recursive-inorder-btree (first (node-children node)))
      (print (node-data node))
      (dfs-recursive-inorder-btree (second (node-children node))))
    (
      (eql (length (node-children node)) 1)
      (dfs-recursive-inorder-btree (first (node-children node)))
      (print (node-data node)))
    (
      (eql (length (node-children node)) 0)
      (print (node-data node)))
    (t
      (print "Invalid binary tree"))))

(defun dfs-stack (node)
  (let ((stack (list node)) (temp Nil))
    (loop while (> (length stack) 0) do
      (print (node-data (first stack)))
      (setq temp (pop stack))
      (loop for child in (node-children temp)
        do (setq stack (nconc stack (list child)))))))

(defun bfs-queue (node)
  (let ((queue (list node)) (temp Nil))
    (loop while (> (length queue) 0) do
      (print (node-data (first queue)))
      (setq temp (pop queue))
      (loop for child in (node-children temp)
        do (setq queue (nconc queue (list child)))))))

(defvar root
  (make-node :data 0 :children
    (list
      (make-node :data 1 :children
        (list
          (make-node :data 4)
          (make-node :data 5)
          (make-node :data 6)))
      (make-node :data 2 :children
        (list
          (make-node :data 7)
          (make-node :data 8)
          (make-node :data 9)))
      (make-node :data 3 :children
        (list
          (make-node :data 10)
          (make-node :data 11)
          (make-node :data 12))))))

(defvar binary-root
  (make-node :data 0 :children
    (list
      (make-node :data 1 :children
        (list (make-node :data 2) (make-node :data 3)))
      (make-node :data 4 :children
        (list (make-node :data 5) (make-node :data 6))))))

(dfs-recursive root)
(dfs-recursive-postorder root)
(dfs-recursive-inorder-btree binary-root)
(dfs-stack root)
(bfs-queue root)
