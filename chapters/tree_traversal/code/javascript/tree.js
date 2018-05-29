function createTree(rows, children) {
  if (rows === 0) {
    return { id: rows, children: [] };
  }

  return {
    id: rows,
    children: Array.from(Array(children).keys(), () => createTree(rows - 1, children))
  };
}

function dfsPreorder(tree) {
  console.log(tree.id);
  tree.children.forEach(dfs);
}

function dfsPostorder(tree) {
  tree.children.forEach(dfs);
  console.log(tree.id);
}

function dfsInorder(tree) {
  if (!tree) {
    return;
  }

  if (tree.children.length > 2) {
    throw new Error("Postorder traversal is only valid for binary trees");
  }

  dfsInorder(tree.children[0]);
  console.log(tree.id);
  dfsInorder(tree.children[1]);
}

function dfsIterative(tree) {
  const stack = [tree];
  while (stack.length > 0) {
    const current = stack.pop();
    console.log(current.id);
    stack.push(...current.children);
  }
}

function bfs(tree) {
  const queue = [tree];
  while (queue.length > 0) {
    const [current] = queue.splice(0, 1);
    console.log(current.id);
    queue.push(...current.children);
  }
}

const root = createTree(3, 3);
dfsRecursive(root);
dfsIterative(root);
bfs(root);
