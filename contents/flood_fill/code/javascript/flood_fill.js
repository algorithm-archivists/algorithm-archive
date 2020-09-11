
function isInBounds(canvas, x, y) {
  return (x >= 0) && (x < canvas[0].length) && (y >= 0) && (y < canvas.length)
}

function color(canvas, x, y, oldColor, newColor) {
  if (isInBounds(canvas, x, y) && canvas[y][x] == oldColor) 
    canvas[y][x] = newColor
}

function findNeighbors(canvas, x, y, oldColor) {
  const allNeighbors = [
    [x, y - 1],  // North
    [x + 1, y],  // East
    [x, y + 1],  // South
    [x - 1, y]   // West
  ]
  
  return allNeighbors
    .filter(loc => isInBounds(canvas, ...loc))
    .filter(loc => canvas[loc[1]][loc[0]] == oldColor)
}

function stackFill(canvas, x, y, oldColor, newColor) {
  const stack = [
    [x, y]
  ]

  while (stack.length > 0) {
    const currentLoc = stack.pop()
    color(canvas, ...currentLoc, oldColor, newColor)

    for(const n of findNeighbors(canvas, ...currentLoc, oldColor))
      stack.push(n)
  }
}

function queueFill(canvas, x, y, oldColor, newColor) {
  const queue = [
    [x, y]
  ]

  while (queue.length > 0) {
    const currentLoc = queue.shift()
    color(canvas, ...currentLoc, oldColor, newColor)

    for (const n of findNeighbors(canvas, ...currentLoc, oldColor)) {
      // Color neighbor pixel before enqueuing to prevent
      // it from being colored multiple times
      color(canvas, ...n, oldColor, newColor)
      queue.push(n)
    }
  }
}

function recursiveFill(canvas, x, y, oldColor, newColor) {
  color(canvas, x, y, oldColor, newColor)

  for(const n of findNeighbors(canvas, x, y, oldColor))
    recursiveFill(canvas, ...n, oldColor, newColor)
}

function copyGrid(canvas) {
  return canvas.map(row => row.map(v => v))
}

function compareGrids(canvas1, canvas2) {
  return canvas1.map((row, y) => {
    return row.every((val, x) => canvas1[y][x] === canvas2[y][x])
  }).every(x => x)
}

let orignal = [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
]

let solutionGrid = [
    [1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
]

let startLoc = [3, 1]

let canvas = copyGrid(orignal)
recursiveFill(canvas, ...startLoc, 0, 1)
console.log(`Recursive Fill: ${compareGrids(canvas, solutionGrid)}`)

canvas = copyGrid(orignal)
stackFill(canvas, ...startLoc, 0, 1)
console.log(`Stackfill: ${compareGrids(canvas, solutionGrid)}`)

canvas = copyGrid(orignal)
queueFill(canvas, ...startLoc, 0, 1)
console.log(`Queuefill: ${compareGrids(canvas, solutionGrid)}`)
