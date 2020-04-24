class Node 
  property id, children 
  def initialize(@id : Int32, @children : Array(Node))
  end 
end 

def dfs_recursive(node) 
  print node.id
  node.children.each{ |child| dfs_recursive child } 
end

def dfs_recursive_postorder(node) 
  node.children.each{ |child| dfs_recursive_postorder child }
  print node.id 
end 

def dfs_recursive_inorder_btree(node) 
  case node.children.size
  when 2
    dfs_recursive_inorder_btree node.children[0]
    print node.id 
    dfs_recursive_inorder_btree node.children[1]  
  when 1 
    dfs_recursive_inorder_btree node.children[0]
    print node.id 
  when 0 
    print node.id 
  else 
    print "Not a binary tree!"
  end 
end 

def dfs_stack(node) 
  stack = [node] 

  until stack.empty? 
    temp = stack.pop 
    print temp.id
    temp.children.each{ |child| stack.push child } 
  end 
end 

def bfs_queue(node) 
  queue = Deque.new [node]

  until queue.empty? 
    temp = queue.shift
    print temp.id 
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

def print_tree(node, depth = [] of String) 
  puts "(#{node.id})"
  depth.push " " 
  len = node.children.size - 1

  (0 .. len).each do |i|
    depth.each{|c| print c} 
    unless i == len 
      print "├" 
      depth.push "│"
      print_tree node.children[i], depth
      depth.pop 
    else 
      print "└"
      depth.push " " 
      print_tree node.children[i], depth
      depth.pop
    end 
  end 
  depth.pop 
end 

def main 
  puts "Creating Tree" 
  root = create_tree levels: 2, num_childs: 3
  print_tree root 

  puts "Using recursive DFS:"
  dfs_recursive root
  puts  

  puts "Using recursive DFS with post-order traversal:" 
  dfs_recursive_postorder root 
  puts  

  puts "Using stack-based DFS:"
  dfs_stack root 
  puts  

  puts "Using queue-based BFS:"
  bfs_queue root 
  puts  

  puts "Creating binary tree to test in-order traversal"
  root_bin = create_tree levels: 3, num_childs: 2
  print_tree root_bin 

  puts "Using In-order DFS:"
  dfs_recursive_inorder_btree root_bin
  puts
end 

main

