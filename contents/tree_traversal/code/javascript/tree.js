function createTree(rows, children) {
  if (rows === 0) {
    return { id: rows, children: [] };
  }

  return {
    id: rows,
    children: [...Array(children).keys()].map(() => createTree(rows - 1, children))
  };
}

function dfsPreorder(tree) {
  process.stdout.write(tree.id + " ");
  tree.children.forEach(dfsPreorder);
}

function dfsPostorder(tree) {
  tree.children.forEach(dfsPostorder);
  process.stdout.write(tree.id + " ");
}

function dfsInorder(tree) {
  if (!tree) {
    return;
  }

  if (tree.children.length > 2) {
    throw new Error("Postorder traversal is only valid for binary trees");
  }

  dfsInorder(tree.children[0]);
  process.stdout.write(tree.id + " ");
  dfsInorder(tree.children[1]);
}

function dfsIterative(tree) {
  const stack = [tree];
  while (stack.length > 0) {
    const current = stack.pop();
    process.stdout.write(current.id + " ");
    stack.push(...current.children);
  }
}

function bfs(tree) {
  const queue = [tree];
  while (queue.length > 0) {
    const current = queue.shift();
    process.stdout.write(current.id + " ");
    queue.push(...current.children);
  }
}

const root = createTree(2, 3);
console.log("[#]\nRecursive DFS:");
dfsPreorder(root);
console.log();
console.log("[#]\nRecursive Postorder DFS:");
dfsPostorder(root);
console.log();
console.log("[#]\nStack-based DFS:");
dfsIterative(root);
console.log();
console.log("[#]\nQueue-based BFS:");
bfs(root);
console.log();
const root_binary = createTree(3, 2);
console.log("[#]\nRecursive Inorder DFS for Binary Tree:");
dfsInorder(root_binary);
console.log();

