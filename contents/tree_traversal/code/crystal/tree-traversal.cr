class Node 
  property id, children 
  def initialize(@id : Int32, @children : Array(Node))
  end 
end 

def dfs_recursive(node) 
  print "#{node.id} "
  node.children.each{ |child| dfs_recursive child } 
end

def dfs_recursive_postorder(node) 
  node.children.each{ |child| dfs_recursive_postorder child }
  print "#{node.id} "
end 

def dfs_recursive_inorder_btree(node) 
  case node.children.size
  when 2
    dfs_recursive_inorder_btree node.children[0]
    print "#{node.id} "
    dfs_recursive_inorder_btree node.children[1]  
  when 1 
    dfs_recursive_inorder_btree node.children[0]
    print "#{node.id} "
  when 0 
    print "#{node.id} "
  else 
    print "Not a binary tree!"
  end 
end 

def dfs_stack(node) 
  stack = [node] 

  until stack.empty? 
    temp = stack.pop 
    print "#{temp.id} "
    temp.children.each{ |child| stack.push child } 
  end 
end 

def bfs_queue(node) 
  queue = Deque.new [node]

  until queue.empty? 
    temp = queue.shift
    print "#{temp.id} "
    temp.children.each{ |child| queue.push child }
  end  
end 

def create_tree(levels, num_childs) 

  children = [] of Node  
  unless levels == 0 
    num_childs.times{children.push create_tree levels-1, num_childs } 
  end 
  
  Node.new(levels, children) 
end

def main 
  root = create_tree levels: 2, num_childs: 3

  puts "[#]\nRecursive DFS:"
  dfs_recursive root
  puts  

  puts "[#]\nRecursive Postorder DFS:" 
  dfs_recursive_postorder root 
  puts  

  puts "[#]\nStack-based DFS:"
  dfs_stack root 
  puts  

  puts "[#]\nQueue-based BFS:"
  bfs_queue root 
  puts  

  root_bin = create_tree levels: 3, num_childs: 2

  puts "[#]\nRecursive Inorder DFS for Binary Tree:"
  dfs_recursive_inorder_btree root_bin
  puts
end 

main

