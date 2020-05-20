def create_tree(rows, children)
  return { id: rows, children: [] } if rows.zero?

  {
    id: rows,
    children: Array.new(3, create_tree(rows - 1, children))
  }
end

def dfs_preorder(tree)
  puts tree[:id]
  tree[:children].each { |child| dfs_preorder(child) }
end

def dfs_postorder(tree)
  tree[:children].each { |child| dfs_postorder(child) }
  puts tree[:id]
end

def dfs_inorder(tree)
  return unless tree

  if tree[:children].count > 2
    raise 'Postorder traversal is only valid for binary trees'
  end

  dfs_inorder(tree.children[0])
  puts tree[:id]
  dfs_inorder(tree.children[1])
end

def dfs_iterative(tree)
  stack = [tree]
  while stack.count.positive?
    current = stack.pop
    puts current[:id]
    stack.push(*current[:children])
  end
end

def bfs(tree)
  queue = [tree]
  while queue.count.positive?
    current = queue.shift
    puts current[:id]
    queue.push(*current[:children])
  end
end

root = create_tree(3, 3)
dfs_preorder(root)
dfs_postorder(root)
dfs_iterative(root)
bfs(root)
