Object subclass: #Node
    instanceVariableNames: 'children data'
    classVariableNames: ''
    package: ''

Node>>children
    "Children getter."
    ^ children

Node>>children: newChildren
    "Children setter."
    children := newChildren.

Node>>data
    "Data getter"
    ^ data

Node>>data: newData
    "Data setter"
    data := newData.

Node>>dfsRecursive
    "Recursive depth first search."
    Transcript show: data; cr.
    children collect: [ :child | child dfsRecursive ]

Node>>dfsRecursivePostOrder
    "Recursive depth first search (post-order)."
    children collect: [ :child | (child dfsRecursivePostOrder)].
    Transcript show: data; cr.
 
Node>>dfsInOrderBinaryTree
    "Recursive depth first search on a binary tree in order."
    children size > 2 ifTrue: [
        Transcript show: 'This is not a binary tree!'; cr.
        ^self.
    ].
    children size = 2 ifTrue: [
        (children at: 1) dfsInOrderBinaryTree: value.
    ].
    Transcript show: data; cr.
    children size >= 1 ifTrue: [
        (children at: 0) dfsInOrderBinaryTree: value.
    ].
    ^self.

Node>>dfsStack
    "Depth-first search with a stack."
    | stack top |
    stack := Stack new.
    stack push: self.
    [stack size > 0] whileTrue: [
        top := stack pop.
	Transcript show: (top data); cr.
        top children reverseDo: [ :child |
            stack push: child.
        ].
    ].

Node>>bfs
    "A breadth-first tree search using queues."
    | queue current |
    queue := LinkedList with: self.
    [ queue size > 0 ] whileTrue: [
        current := queue first.
	queue removeFirst.
	Transcript show: (current data); cr.
	current children collect: [ :child |
	    queue addLast: child
        ].
     ].

| test |
test := Node new: 1 children: { Node new: 2.
                                Node new: 3 children: { Node new: 4.
                                                        Node new: 5. } }.
test dfsRecursive.
test dfsRecursivePostorder.
test dfsInOrderBinaryTree.
test dfsStack.
test bfs.
