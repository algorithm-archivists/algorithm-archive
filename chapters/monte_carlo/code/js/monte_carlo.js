// submitted by xam4lor
function inCircle(xPos, yPos, radius) {
    return xPos * xPos + yPos * yPos < radius * radius;
}

function monteCarlo(n, radius) {
    let piCount = 0;

    for (let i = 0; i < n; i++) {
        const pointX = Math.random();
        const pointY = Math.random();

        if (inCircle(pointX, pointY, radius)) {
            piCount++;
        }
    }

    const piEstimate = 4 * Math.PI / (n * (radius * radius));
    console.log('Percent error is: %s%', piEstimate);
}

monteCarlo(100000000, 0.5);
