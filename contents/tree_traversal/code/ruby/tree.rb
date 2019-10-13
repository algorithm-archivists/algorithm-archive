def create_tree(rows, children)
  return { id: rows, children: [] } if rows.zero?

  {
    id: rows,
    children: Array.new(children, create_tree(rows - 1, children))
  }
end

def dfs_preorder(tree)
  print "#{tree[:id]} "
  tree[:children].each { |child| dfs_preorder(child) }
end

def dfs_postorder(tree)
  tree[:children].each { |child| dfs_postorder(child) }
  print "#{tree[:id]} "
end

def dfs_inorder(tree)
  return unless tree

  if tree[:children].count > 2
    raise 'Postorder traversal is only valid for binary trees'
  end

  dfs_inorder(tree[:children][0])
  print "#{tree[:id]} "
  dfs_inorder(tree[:children][1])
end

def dfs_iterative(tree)
  stack = [tree]
  while stack.count.positive?
    current = stack.pop
    print "#{current[:id]} "
    stack.push(*current[:children])
  end
end

def bfs(tree)
  queue = [tree]
  while queue.count.positive?
    current = queue.shift
    print "#{current[:id]} "
    queue.push(*current[:children])
  end
end

root = create_tree(2, 3)
puts "[#]\nRecursive DFS:"
dfs_preorder(root)
puts ""
puts "[#]\nRecursive Postorder DFS:"
dfs_postorder(root)
puts ""
puts "[#]\nStack-based DFS:"
dfs_iterative(root)
puts ""
puts "[#]\nQueue-based DFS:"
bfs(root)
puts ""
root_binary = create_tree(3, 2);
puts "[#]\nRecursive Inorder DFS for Binary Tree:"
dfs_inorder(root_binary)
