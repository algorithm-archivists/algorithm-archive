use math

# The node has the following definition:
# node = [&id=number &child-num=number &children=[node ..]]

fn make-node [id child-num]{
  put [&id=$id &child-num=$child-num &children=[(repeat $child-num 0)]]
}

fn create-tree [levels child-num]{
  if (== $levels 0) {
    make-node $levels 0
  } else {
    node = (make-node $levels $child-num)
    i = 0
    while (< $i $child-num) {
      node[children][$i] = (create-tree (- $levels 1) $child-num)
      i = (+ $i 1)
    }

    put $node
  }
}

fn dfs-recursive [node]{
  print $node[id]' '

  for child $node[children] {
    dfs-recursive $child
  }
}

fn dfs-recursive-postorder [node]{
  for child $node[children] {
    dfs-recursive-postorder $child
  }

  print $node[id]' '
}

fn dfs-inorder [node]{
  if (== $node[child-num] 2) {
    dfs-inorder $node[children][0]
    print $node[id]' '
    dfs-inorder $node[children][1]
  } elif (== $node[child-num] 1) {
    # this won't ever be true when using `create-tree`
    # to create a binary tree
    dfs-inorder $node[children][0]
    print $node[id]' '
  } else {
    print $node[id]' '
  }
}

# dfs-stack and dfs-queue copy a lot of nodes to the stack/queue
# due to the fact that elvish doesn't have pointers
fn dfs-stack [node]{
  # the maximum number of elements on the stack
  # (works only with `create-tree`)
  stack = [(repeat (+ $node[id] $node[child-num]) 0)]
  stack[0] = $node
  stack-size = 0 # last index in the stack ie. keeps track of the top

  while (!= $stack-size -1) {
    last-node = $stack[$stack-size]
    print $last-node[id]' '
    stack-size = (- $stack-size 1)

    for child $last-node[children] {
      stack-size = (+ $stack-size 1)
      stack[$stack-size] = $child
    }
  }
}

fn dfs-queue [node]{
  # the maximum number of elements in the queue is the
  # number of elemnts on the last row (works only with `create-tree`)
  queue = [(repeat (math:pow $node[child-num] $node[id]) 0)]
  queue[0] = $node
  queue-size = 0

  while (!= $queue-size -1) {
    first-node = $queue[0]
    print $first-node[id]' '

    queue = $queue[1..]

    # keeping the size of the queue constant
    queue = [$@queue 0]

    queue-size = (- $queue-size 1)

    for child $first-node[children] {
      queue-size = (+ $queue-size 1)
      queue[$queue-size] = $child
    }
  }
}

tree = (create-tree 2 3)
print 'dfs-recursive = '
dfs-recursive $tree
echo

print 'dfs-recursive-postorder = '
dfs-recursive-postorder $tree
echo

btree = (create-tree 2 2)
print 'dfs-inorder(binary tree only) = '
dfs-inorder $btree
echo

print 'dfs-stack = '
dfs-stack $tree
echo

print 'dfs-queue = '
dfs-queue $tree
echo
