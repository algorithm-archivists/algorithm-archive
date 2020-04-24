// submitted by xam4lor
function inCircle(xPos, yPos) {
  // Setting radius to 1 for unit circle
  let radius = 1;
  return xPos * xPos + yPos * yPos < radius * radius;
}

function monteCarlo(n) {
  let piCount = 0;

  for (let i = 0; i < n; i++) {
    const pointX = Math.random();
    const pointY = Math.random();

    if (inCircle(pointX, pointY)) {
      piCount++;
    }
  }

  // This is using a quarter of the unit sphere in a 1x1 box.
  // The formula is pi = (boxLength^2 / radius^2) * (piCount / n), but we
  // are only using the upper quadrant and the unit circle, so we can use
  // 4*piCount/n instead
  // piEstimate = 4*piCount/n
  const piEstimate = 4 * piCount / n;
  console.log('Percent error is: %s%', 100 * Math.abs(piEstimate - Math.PI) / Math.PI);
}

monteCarlo(100000000);
