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
  if (!tree) {
    return;
  }

  process.stdout.write(tree.id + " ");
  tree.children.forEach(dfsPreorder);
}

function dfsPostorder(tree) {
  if (!tree) {
    return;
  }

  tree.children.forEach(dfsPostorder);
  process.stdout.write(tree.id + " ");
}

function dfsInorder(tree) {
  if (!tree) {
    return;
  }

  switch (tree.children.length) {
    case 2:
      dfsInorder(tree.children[0]);
      console.log(tree.id);
      dfsInorder(tree.children[1]);
      break;
    case 1:
      dfsInorder(tree.children[0]);
      console.log(tree.id);
      break;
    case 0:
      console.log(tree.id);
      break;
    default:
      throw new Error("Postorder traversal is only valid for binary trees");
  }
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

