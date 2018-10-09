function euclidMod(a, b) {
  a = Math.abs(a);
  b = Math.abs(b);

  let temp;
  while (b !== 0) {
    temp = b;
    b = a % b;
    a = temp;
  }

  return a;
}

function euclidSub(a, b) {
  a = Math.abs(a);
  b = Math.abs(b);

  while (a !== b) {
    if (a > b) {
      a -= a - b;
    } else {
      b = b - a;
    }
  }

  return a;
}

console.log(euclidMod(64 * 67, 64 * 81));
console.log(euclidSub(128 * 12, 128 * 77));
