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
  size_t value;
};

// Simple recursive scheme for DFS
void dfs_recursive(node const& n) {
  // Here we are doing something...
  std::cout << n.value << ' ';
  for (auto const& child : n.children) {
    dfs_recursive(child);
  }
}

void dfs_recursive_postorder(node const& n) {
  for (auto const& child : n.children) {
    dfs_recursive_postorder(child);
  }
  std::cout << n.value << ' ';
}


void dfs_recursive_inorder_btree(node const& n) {
  switch (n.children.size()) {
    case 2:
      dfs_recursive_inorder_btree(n.children[0]);
      std::cout << n.value << ' ';
      dfs_recursive_inorder_btree(n.children[1]);
      break;
    case 1:
      dfs_recursive_inorder_btree(n.children[0]);
      std::cout << n.value << ' ';
      break;
    case 0:
      std::cout << n.value << ' ';
      break;
    default:
      std::cout << "This is not a binary tree.\n";
      break;
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
    std::cout << temp.value << ' ';

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

    std::cout << temp.value << ' ';
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
    return create_tree(num_row - 1, num_child);
  });

  return node{std::move(vec), num_row};
}

int main() {
  // Creating Tree in main
  auto root = create_tree(2, 3);
  auto binary_root = create_tree(3, 2);
  std::cout << "[#]\nRecursive DFS:\n";
  dfs_recursive(root);
  std::cout << '\n';
  std::cout << "[#]\nRecursive Postorder DFS:\n";
  dfs_recursive_postorder(root);
  std::cout << '\n';
  std::cout << "[#]\nStack-based DFS:\n";
  dfs_stack(root);
  std::cout << '\n';
  std::cout << "[#]\nQueue-based BFS:\n";
  bfs_queue(root);
  std::cout << '\n';
  std::cout << "[#]\nRecursive Inorder DFS for Binary Tree:\n";
  dfs_recursive_inorder_btree(binary_root);
  std::cout << '\n';

  return 0;
}
