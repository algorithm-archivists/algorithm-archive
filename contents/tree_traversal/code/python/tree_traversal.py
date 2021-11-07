class Node:
    def __init__(self):
        self.data = None
        self.children = []

def create_tree(node, num_row, num_child):
    node.data = num_row

    if num_row > 0:
        for i in range(num_child):
            child = create_tree(Node(), num_row-1, num_child)
            node.children.append(child)

    return node


def dfs_recursive(node):
    if node.data != None:
        print(node.data, end=' ')

    for child in node.children:
        dfs_recursive(child)


def dfs_recursive_postorder(node):
    for child in node.children:
        dfs_recursive_postorder(child)

    if node.data != None:
        print(node.data, end=' ')


# This assumes only 2 children, but accounts for other possibilities
def dfs_recursive_inorder_btree(node):
    if len(node.children) == 2:
        dfs_recursive_inorder_btree(node.children[0])
        print(node.data, end=' ')
        dfs_recursive_inorder_btree(node.children[1])
    elif len(node.children) == 1:
        dfs_recursive_inorder_btree(node.children[0])
        print(node.data, end=' ')
    elif len(node.children) == 0:
        print(node.data, end=' ')
    else:
        print("Not a binary tree!")


def dfs_stack(node):
    stack = []
    stack.append(node)

    temp = None

    while len(stack) > 0:
        print(stack[-1].data, end=' ')
        temp = stack.pop()

        for child in temp.children:
            stack.append(child)


def bfs_queue(node):
    queue = []
    queue.append(node)

    temp = None

    while len(queue) > 0:
        print(queue[0].data, end=' ')
        temp = queue.pop(0)

        for child in temp.children:
            queue.append(child)


def main():
    tree = create_tree(Node(), 2, 3)

    print("[#]\nRecursive DFS:")
    dfs_recursive(tree)
    print()

    print("[#]\nRecursive Postorder DFS:")
    dfs_recursive_postorder(tree)
    print()

    print("[#]\nStack-based DFS:")
    dfs_stack(tree)
    print()

    print("[#]\nQueue-based BFS:")
    bfs_queue(tree)
    print()

    binary_tree = create_tree(Node(), 3, 2)
    
    print("[#]\nRecursive Inorder DFS for Binary Tree:")
    dfs_recursive_inorder_btree(binary_tree)
    print()

if __name__ == '__main__':
    main()

