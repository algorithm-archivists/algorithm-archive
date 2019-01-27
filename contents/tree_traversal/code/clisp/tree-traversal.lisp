;;;; Tree traversal in Common Lisp

(defstruct node data children)

(defun dfs-recursive (node)
  "A depth first approach for printing out all values in a tree"
  (when (not (eql (node-data node) Nil))
    (print (node-data node)))
  (loop for child in (node-children node)
    do (dfs-recursive child)))

(defun dfs-recursive-postorder (node)
  "A depth first approach for printing out all values in a tree starting from the bottum"
  (loop for child in (node-children node)
    do (dfs-recursive-postorder child))
  (when (not (eql (node-data node) Nil))
    (print (node-data node))))

(defun dfs-recursive-inorder-btree (node)
  "A depth first search approach for printing all values in a binary tree"
  (cond
    ((eql (length (node-children node)) 2)
      (dfs-recursive-inorder-btree (first (node-children node)))
      (print (node-data node))
      (dfs-recursive-inorder-btree (second (node-children node))))
    ((eql (length (node-children node)) 1)
      (dfs-recursive-inorder-btree (first (node-children node)))
      (print (node-data node)))
    ((eql (length (node-children node)) 0)
      (print (node-data node)))
    (t
      (print "Invalid binary tree"))))

(defun dfs-stack (node)
  "A depth first approach for printing out all values in a tree using a stack"
  (let
    ((stack (list node)) (temp Nil))
    (loop while (> (length stack) 0) do
      (print (node-data (first stack)))
      (setf temp (pop stack))
      (loop for child in (node-children temp)
        do (setf stack (nconc stack (list child)))))))

(defun bfs-queue (node)
  "A breadth first search approach for printing out all values in a tree"
  (let ((queue (list node)) (temp Nil))
    (loop while (> (length queue) 0) do
      (print (node-data (first queue)))
      (setf temp (pop queue))
      (loop for child in (node-children temp)
        do (setf queue (nconc queue (list child)))))))

;a tree for testing
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

;a binary tree for testing
(defvar binary-root
  (make-node :data 0 :children
    (list
      (make-node :data 1 :children
        (list (make-node :data 2) (make-node :data 3)))
      (make-node :data 4 :children
        (list (make-node :data 5) (make-node :data 6))))))

;should print: 0, 1, 4, 5, 6, 2, 7, 8, 9, 3, 10, 11, 12
(dfs-recursive root)

;should print: 4, 5, 6, 1, 7, 8, 9, 2, 10, 11, 12, 3, 0
(dfs-recursive-postorder root)

;should print: 2, 1, 3, 0, 5, 4, 6
(dfs-recursive-inorder-btree binary-root)

;should print: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
(dfs-stack root)

;should print: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
(bfs-queue root)
