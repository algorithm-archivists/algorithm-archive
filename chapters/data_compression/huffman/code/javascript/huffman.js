function encode(str) {
  const tree = createTree(str);
  const codebook = createCodebook(tree);
  return {
    string: [...str].map(c => codebook[c]).join(""),
    tree,
    codebook
  };

  function createTree(str) {
    const chars = [...str];
    const charCounts = chars.reduce((counts, char) => {
      counts[char] = (counts[char] || 0) + 1;
      return counts;
    }, {});

    const nodes = Object.entries(charCounts).map(([key, weight]) => ({ key, weight }));
    const priorityQueue = makeQueue(nodes);
    while (priorityQueue.data.length > 1) {
      const left = priorityQueue.dequeue();
      const right = priorityQueue.dequeue();
      priorityQueue.enqueue({ weight: left.weight + right.weight, left, right });
    }
    return priorityQueue.dequeue();
  }

  function createCodebook(tree) {
    return recurse(tree, "", {});

    function recurse(node, bitstring, dict) {
      if (!node.left && !node.right) {
        dict[node.key] = bitstring;
      } else {
        if (node.left) {
          recurse(node.left, bitstring + "0", dict);
        }

        if (node.right) {
          recurse(node.right, bitstring + "1", dict);
        }
      }
      return dict;
    }
  }
}

function decode(bitstring, tree) {
  const result = [];
  let node = tree;

  for (const bit of [...bitstring]) {
    node = bit === "0" ? node.left : node.right;
    if (!node.left && !node.right) {
      result.push(node.key);
      node = tree;
    }
  }

  return result.join("");
}

// This queue implementation is horribly inefficient, but a proper, heap-based implementation would
// be longer that the algorithm itself
function makeQueue(iterable) {
  return {
    data: [...iterable].sort((a, b) => a.weight - b.weight),
    enqueue(value) {
      const target = this.data.findIndex(x => x.weight > value.weight);
      if (target === -1) {
        this.data.push(value);
      } else {
        this.data = [...this.data.slice(0, target), value, ...this.data.slice(target)];
      }
    },
    dequeue() {
      return this.data.shift();
    }
  };
}

const encoded = encode("bibbity bobbity");
const decoded = decode(encoded.string, encoded.tree);
console.log(encoded.string);
console.log(decoded);
