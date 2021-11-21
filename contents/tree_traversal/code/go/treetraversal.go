package main

import "fmt"

type node struct {
	id       int
	children []*node
}

func dfsRecursive(n *node) {
    fmt.Printf("%d ", n.id)
	for _, child := range n.children {
		dfsRecursive(child)
	}
}

func dfsRecursivePostorder(n *node) {
	for _, child := range n.children {
		dfsRecursivePostorder(child)
	}
	fmt.Printf("%d ", n.id)
}

func dfsRecursiveInorderBtree(n *node) {
	switch len(n.children) {
	case 2:
		dfsRecursiveInorderBtree(n.children[0])
		fmt.Printf("%d ", n.id)
		dfsRecursiveInorderBtree(n.children[1])
	case 1:
		dfsRecursiveInorderBtree(n.children[0])
		fmt.Printf("%d ", n.id)
	case 0:
		fmt.Printf("%d ", n.id)
	default:
		fmt.Println("This is not a binary tree")
	}
}

func dfsStack(n *node) {
	stack := []*node{n}

	for len(stack) > 0 {
		cur := stack[0]
		stack = stack[1:]
		fmt.Printf("%d ", cur.id)
		stack = append(cur.children, stack...)
	}
}

func bfsQueue(n *node) {
	queue := []*node{n}

	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		fmt.Printf("%d ", cur.id)
		queue = append(queue, cur.children...)
	}
}

func createTree(numRow, numChild int) *node {
	if numRow == 0 {
		return &node{id: 0}
	}

	cur := new(node)
	cur.id = numRow

	for x := 0; x < numChild; x++ {
		cur.children = append(cur.children, createTree(numRow-1, numChild))
	}
	return cur
}

func main() {
	root := createTree(2, 3)
	binTree := createTree(3, 2)

    fmt.Println("[#]\nRecursive DFS:")
	dfsRecursive(root)
    fmt.Println()

    fmt.Println("[#]\nRecursive Postorder DFS:")
	dfsRecursivePostorder(root)
    fmt.Println()

    fmt.Println("[#]\nStack-based DFS:")
	dfsStack(root)
    fmt.Println()

    fmt.Println("[#]\nQueue-based BFS:")
	bfsQueue(root)
    fmt.Println()

    fmt.Println("[#]\nRecursive Inorder DFS for Binary Tree:")
	dfsRecursiveInorderBtree(binTree)
    fmt.Println()

}
