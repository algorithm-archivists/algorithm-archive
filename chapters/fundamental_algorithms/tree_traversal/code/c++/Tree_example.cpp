// initially contributed by James Schloss (Leios)
// restyled by Nicole Mazzuca (ubsan)

#include <iostream>
#include <cstddef>
#include <utility>
#include <vector>
#include <stack>
#include <queue>

using std::size_t;

class Tree {
    std::vector<Tree> children;
    int value;
public:
    Tree(int value): children(), value(value) {}

    void add_child(Tree child)
    {
        children.push_back(std::move(child));
    }

    // Simple recursive scheme for DFS
    void DFS_recursive() const {
        // Here we are doing something...
        std::cout << value << '\n';
        for (Tree const& child: children) {
            child.DFS_recursive();
        }
    }

    // Simple non-recursive scheme for DFS
    void DFS_stack() const {
        std::stack<const Tree*> stack;
        stack.push(this);

        while (stack.size() > 0) {
            const Tree& temp = *stack.top();
            stack.pop();

            std::cout << temp.value << '\n';
            for (Tree const& child: temp.children) {
                stack.push(&child);
            }
        }
    }

    // simple non-recursive scheme for BFS
    void BFS_queue() const {
        std::queue<const Tree*> queue;
        queue.push(this);

        while (queue.size() > 0) {
            const Tree& temp = *queue.front();
            queue.pop();

            std::cout << temp.value << '\n';
            for (Tree const& child: temp.children) {
                queue.push(&child);
            }
        }
    }
};

Tree create_tree(size_t num_row, size_t num_child)
{
    // We'll just set the ID to whatever we want here...
    Tree ret(num_row);

    if (num_row == 0) {
        return ret;
    }

    // Creating children
    for (size_t i = 0; i < num_child; ++i) {
        Tree child = create_tree(num_row - 1, num_child);
        ret.add_child(std::move(child));
    }

    return ret;
}


int main() {
    // Creating Tree in main
    Tree root = create_tree(3, 3);
    root.DFS_recursive();
    root.DFS_stack();
    root.BFS_queue();
}

