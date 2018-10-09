package main

import "fmt"

type node struct {
	id       int
	children []*node
}

func dfsRecursive(n *node) {
	fmt.Println(n.id)
	for _, child := range n.children {
		dfsRecursive(child)
	}
}

func dfsRecursivePostorder(n *node) {
	for _, child := range n.children {
		dfsRecursive(child)
	}
	fmt.Println(n.id)
}

func dfsRecursiveInorderBtree(n *node) {
	switch len(n.children) {
	case 2:
		dfsRecursiveInorderBtree(n.children[0])
		fmt.Println(n.id)
		dfsRecursiveInorderBtree(n.children[1])
	case 1:
		dfsRecursiveInorderBtree(n.children[0])
		fmt.Println(n.id)
	case 0:
		fmt.Println(n.id)
	default:
		fmt.Println("This is not a binary tree")
	}
}

func dfsStack(n *node) {
	stack := []*node{n}

	for len(stack) > 0 {
		cur := stack[0]
		stack = stack[1:]
		fmt.Println(cur.id)
		stack = append(cur.children, stack...)
	}
}

func bfsQueue(n *node) {
	queue := []*node{n}

	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		fmt.Println(cur.id)
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
	root := createTree(3, 3)
	binTree := createTree(3, 2)

	fmt.Println("DFS recursive:")
	dfsRecursive(root)
	fmt.Println("DFS post order recursive:")
	dfsRecursivePostorder(root)
	fmt.Println("DFS inorder binary tree:")
	dfsRecursiveInorderBtree(binTree)
	fmt.Println("DFS stack:")
	dfsStack(root)
	fmt.Println("BFS queue:")
	bfsQueue(root)
}
