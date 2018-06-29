// submitted by xam4lor
function inCircle(xPos, yPos, radius) {
    if(xPos * xPos + yPos * yPos < radius * radius) {
        return true;
    } else {
        return false;
    }
}

function monteCarlo(n, radius) {
    let piCount = 0;

    for (var i = 1; i < n; i++) {
        let pointX = Math.random();
        let pointY = Math.random();

        if(inCircle(pointX, pointY, radius)) {
            piCount++;
        }
    }

    let piEstimate = 4 * Math.PI / (n * (radius * radius));
    console.log('Percent error is: %s%', piEstimate);
}

monteCarlo(100000000, 0.5);
