struct node {
  std::vector<node> children;
  int id;
};

void dfs_recursive(const node& n) {
  // Here we are doing something...
  std::cout << n.id << '\n';

  for (auto const& child : n.children) {
    dfs_recursive(child);
  }
}

void DFS_recursive_postorder(const node& n) {
  for (auto const& child : n.children) {
    dfs_recursive_postorder(child);
  }

  // Here we are doing something...
  std::cout << n.id << '\n';
}

// This assumes only 2 children
void dfs_recursive_inorder_bnode(const node& n) {
  if (not n.children.empty()) {
    if (n.children.size() != 2) {
      std::cerr << "Not binary tree!\n";
      std::abort();
    }
    dfs_recursive_inorder_bnode(n.children[0]);
    std::cout << n.id << '\n';
    dfs_recursive_inorder_bnode(n.children[1]);
  } else {
    std::cout << n.id << '\n';
  }
}

void dfs_stack(const node& n) {
  std::stack<node> s;
  s.push(n);

  while (s.size() > 0) {
    node temp = s.top();
    s.pop();
    std::cout << s.top().id << '\n';

    for (auto const& child : temp.children) {
      s.push(child);
    }
  }
}

void bfs_queue(const node& n) {
  std::queue<node> q;
  q.push(n);

  while (q.size() > 0) {
    node temp = q.front();
    q.pop();

    std::cout << q.front().id << '\n';
    for (auto const& child : temp.children) {
      q.push(child);
    }
  }
}
