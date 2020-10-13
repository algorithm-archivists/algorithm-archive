// This is a p5.js sketch.
// https://p5js.org

function isInBounds(x, y) {
  return (x >= 0) && (x < width) && (y >= 0) && (y < height)
}

function isColor(x, y, color) {
  const index = (x + y * width) * 4
  return pixels[index] == red(color) &&
    pixels[index + 1] == green(color) &&
    pixels[index + 2] == blue(color) &&
    pixels[index + 3] == alpha(color)
}

function recolorPixel(x, y, oldColor, newColor) {
  const index = (x + y * width) * 4
  pixels[index] = red(newColor)
  pixels[index + 1] = green(newColor)
  pixels[index + 2] = blue(newColor)
  pixels[index + 3] = alpha(newColor)
}

function findNeighbors(x, y, oldColor) {
  const allNeighbors = [
    [x, y - 1],  // North
    [x + 1, y],  // East
    [x, y + 1],  // South
    [x - 1, y]   // West
  ]
  
  return allNeighbors
    .filter(loc => isInBounds(...loc))
    .filter(loc => isColor(...loc, oldColor))
}

function stackFill(x, y, oldColor, newColor) {
  const stack = [
    [x, y]
  ]

  while (stack.length > 0) {
    const currentLoc = stack.pop()
    recolorPixel(...currentLoc, oldColor, newColor)

    for (const n of findNeighbors(...currentLoc, oldColor))
      stack.push(n)
  }
}

function queueFill(x, y, oldColor, newColor) {
  const queue = [
    [x, y]
  ]

  while (queue.length > 0) {
    const currentLoc = queue.shift()
    recolorPixel(...currentLoc, oldColor, newColor)

    for (const n of findNeighbors(...currentLoc, oldColor)) {
      // Color neighbor pixel before enqueuing to prevent
      // it from being colored multiple times
      recolorPixel(...n, oldColor, newColor)
      queue.push(n)
    }
  }
}

function recursiveFill(x, y, oldColor, newColor) {
  recolorPixel(x, y, oldColor, newColor)

  for (const n of findNeighbors(x, y, oldColor))
    recursiveFill(...n, oldColor, newColor)
}

function setup() {
  createCanvas(600, 200);
  noLoop()
}

function draw() {
  const bgColor = color(0, 0, 0, 0)
  background(bgColor);

  noFill()
  stroke(0)
  strokeWeight(3)
  triangle(80, 80, 160, 100, 100, 140)
  triangle(340, 30, 380, 170, 230, 120)
  triangle(480, 40, 580, 140, 430, 160)

  loadPixels()
  
  // recursiveFill tends to blow up the stack for large 
  // areas so it gets a smaller triangle
  recursiveFill(100, 100, bgColor, color(30, 245, 250))
  stackFill(300, 100, bgColor, color(130, 50, 180))
  queueFill(500, 100, bgColor, color(50, 255, 225))
  
  updatePixels()
}