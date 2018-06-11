# Depth-First and Breadth-First Traversal
# Submitted by Matthew Giallourakis
from collections import deque

class node():
    "Create the node structure"
    def __init__(self, value, left, right):
        self.value = value
        self.left = left
        self.right = right

def make_tree(root,nums):
    "Makes a binary search tree from a list of numbers"
    for num in nums:
        temp_node = root
        new_node = node(value=num, left=None, right=None)
        while True:
            if num < temp_node.value:
                if temp_node.left is None:
                    temp_node.left = new_node
                    break
                else:
                    temp_node = temp_node.left
            elif num > temp_node.value:
                if temp_node.right is None:
                    temp_node.right = new_node
                    break
                else:
                    temp_node = temp_node.right

def depth_first_search(root):
    "Traverses through a tree depth-first by putting nodes on a stack"
    stack = deque()
    result_list = []
    stack.append(root)
    while len(stack) != 0:
        # Take off the node at the top of the stack, add it to the list
        temp_node = stack.pop()
        result_list.append(temp_node.value)
        # Add the children of that node to the top of the stack
        if temp_node.right is not None:
            stack.append(temp_node.right)
        if temp_node.left is not None:
            stack.append(temp_node.left)
    return result_list


def breadth_first_search(root):
    "Traverses through a tree breadth-first by putting nodes into a queue"
    queue = deque()
    result_list = []
    queue.append(root)
    while len(queue) != 0:
        # Take off the node at the top of the queue, add it to the list
        temp_node = queue.pop()
        result_list.append(temp_node.value)
        # Add the children of that node to the bottom of the queue
        if temp_node.left is not None:
            queue.appendleft(temp_node.left)
        if temp_node.right is not None:
            queue.appendleft(temp_node.right)
    return result_list


def main():
    nums = [5,8,6,9,2,1,3]
    root = node(value=nums.pop(0),left=None,right=None)
    make_tree(root,nums)
    #     Tree Structure
    #           5
    #       2       8
    #     1   3   6   9
    #

    print("Depth First:",depth_first_search(root))
    # prints [5, 2, 1, 3, 8, 6, 9]

    print("Breadth First:",breadth_first_search(root))
    # prints [5, 2, 8, 1, 3, 6, 9]


if __name__ == '__main__':
    main()

