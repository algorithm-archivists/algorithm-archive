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

Node>>dfsRecursive: value
    "Recursive depth first search."
    data = value ifTrue: [ 
    	^true.
    ].
    children collect: [ :child |
        (child dfsRecursive: value) ifTrue: [ ^true. ]
    ]
    ^false.

Node>>dfsRecursivePostOrder: value
    "Recursive depth first search (post-order)."
    children collect: [ :child |
        (child dfsRecursive: value) ifTrue: [ ^true. ]
    ].
    ^(data = value).
 
Node>>dfsInOrderBinaryTree: value
    "Recursive depth first search on a binary tree in order."
    children size > 2 ifTrue: [
        Transcript show: 'This is not a binary tree!'; cr.
        ^self.
    ].
    children size = 2 ifTrue: [
        (children at: 1) dfsInOrderBinaryTree: value.
    ]
    data = value ifTrue: [
        Transcript show: data; cr.
    ]
    children size >= 1 ifTrue: [
        (children at: 0) dfsInOrderBinaryTree: value.
    ]
    ^self.

Node>>dfsStack: value
    "Depth-first search with a stack."
    | stack top |
    stack := Stack new.
    stack push: self.
        [stack size > 0] whileTrue: [
        top := stack pop.
	top data = value ifTrue: [
            ^true.
	].
        top children reverseDo: [ :child |
            stack push: child.
        ].
    ].
    ^false

Node>>bfs: value
    "A breadth-first tree search using queues."
    | queue current |
    queue := LinkedList with: self.
    [ queue size > 0 ] whileTrue: [
        current := queue first.
	queue removeFirst.
	current data = value ifTrue: [
            ^true.
	]
	current children collect: [ :child |
	    queue addLast: child
        ].
     ].
     ^false

| test |
test := Node new: 1 children: { Node new: 2.
                                Node new: 3 children: { Node new: 4.
                                                        Node new: 5. } }.
Transcript show: (test dfsRecursive: 6); cr.
Transcript show: (test dfsRecursivePostorder: 4); cr.
test dfsInOrderBinaryTree: 7.
Transcript show: (test dfsStack: 2); cr.
Transcript show: (test bfs: 4); cr.
