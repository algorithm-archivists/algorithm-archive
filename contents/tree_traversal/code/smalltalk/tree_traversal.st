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
    data isNil ifFalse: [ 
        Transcript show: data.
        Transcript cr.
    ].
    children collect: [ :each | each dfsRecursive].

Node>>dfsRecursivePostorder
    "Recursive depth first search (post-order)."
    children collect: [ :each | each dfsRecursivePostorder ].
    data isNil ifTrue: [ ^ self ].
    Transcript show: data.
    Transcript cr.

Node>>dfsInOrderBinaryTree
    "Recursive depth first search on a binary tree in order."
    children size = 2 ifTrue: [ 
    (children at: 1) dfsInOrderBinaryTree.
        Transcript show: data.
        Transcript cr.
        (children at: 2) dfsInOrderBinaryTree.
        ^self.
    ].
    children size = 1 ifTrue: [ 
        (children at: 1) dfsInOrderBinaryTree.
        Transcript show: data.
	Transcript cr.
	^self.
     ].
     children size = 0 ifTrue: [ 
        Transcript show: data.
        Transcript cr.
        ^self.
     ].
     Transcript show: 'This is not a binary tree!'.
     Transcript cr.
     children length 

Node>>dfsStack
    "Depth-first search with a stack."
    | stack top |
    stack := Stack new.
    stack push: self.
        [stack size > 0] whileTrue: [
        top := stack pop.
        Transcript show: top data.
        Transcript cr.
        top children reverseDo: [ :child |
            stack push: child
        ].
    ].

Node>>bfs
    "A breadth-first tree search using queues."
    | queue current |
    queue := LinkedList with: self.
    [ queue size > 0 ] whileTrue: [
        current := queue first.
	queue removeFirst.
	Transcript show: current.
	Transcript cr.
	current children collect: [ :child |
	    queue addLast: child
        ].
     ]

| test |
test := Node new: 1 children: { Node new: 2.
                                Node new: 3 children: { Node new: 4.
                                                        Node new: 5. } }.
test dfsRecursive.
Transcript cr.
test dfsRecursivePostorder.
Transcript cr.
test dfsInOrderBinaryTree.
Transcript cr.
test dfsStack.
Transcript cr.
test bfs.
