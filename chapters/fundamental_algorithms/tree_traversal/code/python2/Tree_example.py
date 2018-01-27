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

def DFS_recursive(node):
    if len(node.children) > 0:
        print node.data

        for child in node.children:
            DFS_recursive(child)

def DFS_stack(node):
    stack = []
    stack.append(node)

    temp = None

    while len(stack) > 0:
        print stack[-1].data
        temp = stack.pop()

        for child in temp.children:
            stack.append(child)

def BFS_queue(node):
    queue = []
    queue.append(node)

    temp = None

    while len(queue) > 0:
        print queue[0].data
        temp = queue.pop(0)

        for child in temp.children:
            queue.append(child)

def main():
    tree = create_tree(Node(), 3, 3)

    print "Recursive:"
    DFS_recursive(tree)

    print "Stack:"
    DFS_stack(tree)

    print "Queue:"
    BFS_queue(tree)

main()

