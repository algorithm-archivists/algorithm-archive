class Node {
    var value: Int
    var children: [Node]?

    init(value: Int, children: [Node]) {
        self.value = value
        self.children = children
    }
}

func createTree(numRows: Int, numChildren: Int) -> Node {
    let node = Node(value: numRows, children: [])

    if numRows > 0 {
        for _ in 1...numChildren {
            let child = createTree(numRows: numRows-1, numChildren: numChildren)
            node.children?.append(child)
        }
    }

    return node
}

func dfsRecursive(node: Node) {
    print(node.value, terminator:" ")

    for child in node.children! {
        dfsRecursive(node: child)
    }
}

func dfsRecursivePostOrder(node: Node) {
    for child in node.children! {
        dfsRecursivePostOrder(node: child)
    }

    print(node.value, terminator:" ")
}

func dfsRecursiveInOrderBinary(node: Node) {
    if node.children?.count == 2 {
        dfsRecursiveInOrderBinary(node: node.children![0])
        print(node.value, terminator:" ")
        dfsRecursiveInOrderBinary(node: node.children![1])
    } else if node.children?.count == 1 {
        dfsRecursiveInOrderBinary(node: node.children![0])
        print(node.value, terminator:" ")
    } else if node.children?.count == 0 {
        print(node.value, terminator:" ")
    } else {
        print("Not a binary tree!")
    }
}

func dfsStack(node: Node) {
    var stack = [node]
    var temp: Node

    while stack.count > 0 {
        temp = stack.popLast()!
        print(temp.value, terminator:" ")

        for child in temp.children! {
            stack.append(child)
        }
    }
}

func bfsQueue(node: Node) {
    var queue = [node]
    var temp: Node

    while queue.count > 0 {
        temp = queue.remove(at: 0)
        print(temp.value, terminator:" ")

        for child in temp.children! {
            queue.append(child)
        }
    }
}

func main() {
    let root = createTree(numRows: 2, numChildren: 3)

    print("[#]\nRecursive DFS:")
    dfsRecursive(node: root)
    print()
    
    print("[#]\nRecursive Postorder DFS:")
    dfsRecursivePostOrder(node: root)
    print()

    print("[#]\nStack-based DFS:")
    dfsStack(node: root)
    print()

    print("[#]\nQueue-based BFS:")
    bfsQueue(node: root)
    print()

    let rootBinary = createTree(numRows: 3, numChildren: 2)

    print("[#]\nRecursive Inorder DFS for Binary Tree:")
    dfsRecursiveInOrderBinary(node: rootBinary)
    print()
}

main()
