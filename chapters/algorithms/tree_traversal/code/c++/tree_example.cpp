// initially contributed by James Schloss (Leios)
// restyled by Nicole Mazzuca (ubsan)

#include <algorithm>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <queue>
#include <stack>
#include <utility>
#include <vector>

using std::size_t;

struct node {
  std::vector<node> children;
  int value;
};

// Simple recursive scheme for DFS
void dfs_recursive(node const& n) {
  // Here we are doing something...
  std::cout << n.value << '\n';
  for (auto const& child : n.children) {
    dfs_recursive(child);
  }
}

// Simple non-recursive scheme for DFS
void dfs_stack(node const& n) {
  // this stack holds pointers into n's `children` vector,
  // or its children's `children` vector.
  std::stack<node const*> stack;
  stack.push(&n);

  while (stack.size() > 0) {
    auto const& temp = *stack.top();
    stack.pop();
    std::cout << temp.value << '\n';

    for (auto const& child : temp.children) {
      stack.push(&child);
    }
  }
}

// simple non-recursive scheme for BFS
void bfs_queue(node const& n) {
  std::queue<node const*> queue;
  queue.push(&n);

  while (queue.size() > 0) {
    auto const& temp = *queue.front();
    queue.pop();

    std::cout << temp.value << '\n';
    for (auto const& child : temp.children) {
      queue.push(&child);
    }
  }
}

node create_tree(size_t num_row, size_t num_child) {
  if (num_row == 0) {
    return node{std::vector<node>(), 0};
  }

  std::vector<node> vec;
  std::generate_n(std::back_inserter(vec), num_child, [&] {
    return create_node(num_row - 1, num_child);
  });

  return node{std::move(vec), num_row};
}

int main() {
  // Creating Tree in main
  auto root = create_node(3, 3);
  dfs_recursive(root);
  dfs_stack(root);
  bfs_queue(root);
}
