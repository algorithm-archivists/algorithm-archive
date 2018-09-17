class Node {
  constructor(id, children=[]) {
    this.id = id;
    this.children = children;
  }
}

function createTree(numRow, numChild) {
  var root = new Node(numRow);

  if (numRow > 0) {
    for (var i = 0; i < numChild; i++) {
      root.children.push(createTree(numRow-1, numChild));
    }
  }

  return root;
}

function dfsPreorder(tree) {
  console.log(tree.id);
  tree.children.forEach(dfsPreorder);
}

function dfsPostorder(tree) {
  tree.children.forEach(dfsPostorder);
  console.log(tree.id);
}

function dfsInorder(tree) {
  if (!tree) { return; }

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
    const current = queue.shift();
    console.log(current.id);
    queue.push(...current.children);
  }
}

const root = createTree(3, 3)
dfsPreorder(root);
dfsPostorder(root);
dfsIterative(root);
bfs(root);
