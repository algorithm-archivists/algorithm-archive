;;;; Tree traversal in Common Lisp

(defstruct node data children)
	 
(defun dfs-recursive (node)
  "A depth first approach for printing out all values in a tree."
  (when (not (eql (node-data node) nil))
    (print (node-data node)))
  (loop for child in (node-children node)
     do (dfs-recursive child)))

(defun dfs-recursive-postorder (node)
  "A depth first approach for printing out all values in a tree starting from the bottom."
  (loop for child in (node-children node)
     do (dfs-recursive-postorder child))
  (when (not (eql (node-data node) nil))
    (print (node-data node))))

(defun dfs-recursive-inorder-btree (node)
  "A depth first search approach for printing all values in a binary tree."
  (case (length (node-children node))
    (2
     (dfs-recursive-inorder-btree (first (node-children node)))
     (print (node-data node))
     (dfs-recursive-inorder-btree (second (node-children node))))
    (1
     (dfs-recursive-inorder-btree (first (node-children node)))
     (print (node-data node)))
    (0
     (print (node-data node)))
    (t
     (print "Invalid binary tree."))))

(defun dfs-stack (node)
  "A depth first approach for printing out all values in a tree using a stack."
  (let
      ((stack (list node))
       (temp nil))
    (loop while (> (length stack) 0)
       do (print (node-data (first stack)))
	 (setf temp (pop stack))
	 (loop for child in (node-children temp)
	    do (push child stack)))))

(defun bfs-queue (node)
  "A breadth first search approach for printing out all values in a tree."
  (let
      ((queue (list node))
       (temp nil))
    (loop while (> (length queue) 0)
       do (print (node-data (first queue)))
	     (setf temp (pop queue))
	     ;; If the queue is empty, the queue should be filled with the children nodes.
	     (if (eql queue nil)
	         (setf queue (node-children temp))
	         (nconc queue (node-children temp))))))

(defun make-tree (num-rows num-child)
  "Creates a simple tree, where every node has 'num-child' children and is 'num-rows' deep."
  ;; A tree with 0 rows can't be created.
  (if (eql num-rows 1)
      (make-node
       :data 1
       :children nil)
      (make-node
       :data num-rows
       :children (loop repeat num-child collect (make-tree (1- num-rows) num-child)))))

;; A tree for testing
(defvar tree (make-tree 3 3))

;; A binary tree for testing
(defvar binary-tree (make-tree 3 2))

;; Should print: 3, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1
(dfs-recursive tree)

;; Should print: 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 3
(dfs-recursive-postorder tree)

;; Should print: 1, 2, 1, 3, 1, 2, 1
(dfs-recursive-inorder-btree binary-tree)

;; Should print: 3, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1
(dfs-stack tree)

;; Should print: 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1
(bfs-queue tree)
