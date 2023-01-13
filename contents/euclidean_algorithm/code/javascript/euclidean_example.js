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
      a -= b;
    } else {
      b -= a;
    }
  }

  return a;
}

console.log('[#]\nModulus-based euclidean algorithm result:')
console.log(euclidMod(64 * 67, 64 * 81));
console.log('[#]\nSubtraction-based euclidean algorithm result:')
console.log(euclidSub(128 * 12, 128 * 77));
