package main

import (
	"fmt"
	"sort"
)

type node struct {
	freq  int
	char  rune
	left  *node
	right *node
}

type codebook map[rune]string

func buildTree(message string) *node {
	freqMap := make(map[rune]*node)
	nodeList := make([]*node, 0)

	for _, char := range message {
		if _, ok := freqMap[char]; ok {
			freqMap[char].freq++
		} else {
			newNode := new(node)
			newNode.freq = 1
			newNode.char = char
			freqMap[char] = newNode
			nodeList = append(nodeList, newNode)
		}
	}

	for len(nodeList) > 1 {
		sort.Slice(nodeList, func(i, j int) bool {
			return nodeList[i].freq < nodeList[j].freq
		})

		left := nodeList[0]
		right := nodeList[1]
		nodeList = nodeList[2:]
		branch := new(node)
		nodeList = append(nodeList, branch)
		branch.left = left
		branch.right = right
		branch.freq = left.freq + right.freq
	}

	return nodeList[0]
}

func codebookRecurse(node *node, cb *codebook, code []rune) {
	if node == nil {
		return
	}

	if node.left == nil && node.right == nil {
		(*cb)[node.char] = string(code)
	}

	// 0x30 == "0" and 0x31 == "1"
	code = append(code, 0x30)
	codebookRecurse(node.left, cb, code)
	code = append(code[:len(code)-1], 0x31)
	codebookRecurse(node.right, cb, code)
}

func encode(message string) (string, *node, codebook) {
	ret := ""
	root := buildTree(message)
	cb := generateCodebook(root)
	for _, char := range message {
		ret += cb[char]
	}

	return ret, root, cb
}

func decode(message string, root *node) string {
	cur := root
	ret := ""

	for _, char := range message {
		if cur == nil {
			return message
		}

		switch string(char) {
		case "0":
			if cur.left == nil {
				ret += string(cur.char)
				cur = root.left
			} else {
				cur = cur.left
			}
		case "1":
			if cur.right == nil {
				ret += string(cur.char)
				cur = root.right
			} else {
				cur = cur.right
			}
		}
	}

	if cur.char != 0 {
		ret += string(cur.char)
	}

	return ret
}

func generateCodebook(root *node) codebook {
	cb := make(codebook)
	codeArr := make([]rune, 0)
	codebookRecurse(root, &cb, codeArr)
	return cb
}

func main() {
	enc, root, cb := encode("bibbity_bobbity")
	fmt.Println("Codebook:")
	for r, c := range cb {
		fmt.Println(string(r), "->", c)
	}
	fmt.Println("\nEncoded:", enc)
	fmt.Println("Decoded:", decode(enc, root))
}
