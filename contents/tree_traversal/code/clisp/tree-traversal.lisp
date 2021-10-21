;;;; Tree traversal in Common Lisp

(defstruct node data children)

(defun dfs-recursive (node)
  "A depth first approach for printing out all values in a tree."
  (when (node-data node)
    (format t "~a " (node-data node)))
  (loop for child in (node-children node) do
    (dfs-recursive child)))

(defun dfs-recursive-postorder (node)
  "A depth first approach for printing out all values in a tree starting from the bottom."
  (loop for child in (node-children node) do
    (dfs-recursive-postorder child))
  (when (node-data node)
    (format t "~a "  (node-data node))))

(defun dfs-recursive-inorder-btree (node)
  "A depth first search approach for printing all values in a binary tree."
  (case (length (node-children node))
    (2
     (dfs-recursive-inorder-btree (first (node-children node)))
     (format t "~a " (node-data node))
     (dfs-recursive-inorder-btree (second (node-children node))))
    (1
     (dfs-recursive-inorder-btree (first (node-children node)))
     (format t "~a " (node-data node)))
    (0
     (format t "~a " (node-data node)))
    (t
     (print "Invalid binary tree."))))

(defun dfs-stack (node)
  "A depth first approach for printing out all values in a tree using a stack."
  (loop
    with stack = (list node)
    with temp = nil
    while (> (length stack) 0) do
    (format t "~a " (node-data (first stack)))
    (setf temp (pop stack))
    (loop for child in (node-children temp) do
      (push child stack))))

(defun bfs-queue (node)
  "A breadth first search approach for printing out all values in a tree."
  (loop
    with queue = (list node)
    with temp = nil
    while (> (length queue) 0) do
    (format t "~a " (node-data (first queue)))
    (setf temp (pop queue))
    ;; If the queue is empty, the queue should be filled with the children nodes.
    (if (eql queue nil)
        (setf queue (node-children temp))
        (nconc queue (node-children temp)))))

(defun make-tree (num-rows num-child)
  "Creates a simple tree, where every node has 'num-child' children and is 'num-rows' deep."
  ;; A tree with 0 rows can't be created.
  (if (eql num-rows 0)
      (make-node
       :data 0
       :children nil)
      (make-node
       :data num-rows
       :children (loop repeat num-child collect (make-tree (1- num-rows) num-child)))))

;; A tree for testing
(defvar tree (make-tree 2 3))

;; A binary tree for testing
(defvar binary-tree (make-tree 3 2))

;; Should print: 3 2 1 1 1 2 1 1 1 2 1 1 1
(format t "[#]~%Recursive DFS:~%")
(dfs-recursive tree)
(format t "~%")

;; Should print: 1 1 1 2 1 1 1 2 1 1 1 2 3
(format t "[#]~%Recursive Postorder DFS:~%")
(dfs-recursive-postorder tree)
(format t "~%")

;; Should print: 3 2 1 1 1 2 1 1 1 2 1 1 1
(format t "[#]~%Stack-based DFS:~%")
(dfs-stack tree)
(format t "~%")

;; Should print: 3 2 2 2 1 1 1 1 1 1 1 1 1
(format t "[#]~%Queue-based BFS:~%")
(bfs-queue tree)
(format t "~%")

;; Should print: 1 2 1 3 1 2 1
(format t "[#]~%Recursive Inorder DFS for Binary Tree:~%")
(dfs-recursive-inorder-btree binary-tree)
(format t "~%")
