// using recursion
function getGCD(a, b) {
  if (b == 0) return a
  else return getGCD(b, a % b )
}
console.log(getGCD(24, 27)) // 3
