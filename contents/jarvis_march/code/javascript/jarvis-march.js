function jarvisMarch(points) {
  const hull = [];

  let pointOnHull = points.reduce((leftmost, current) => leftmost.x < current.x ? leftmost : current);
  do {
    hull.push(pointOnHull);
    pointOnHull = points.reduce(chooseNextPointOnHull(pointOnHull));
  } while (pointOnHull !== hull[0]);

  return hull;
}

function chooseNextPointOnHull(currentPoint) {
  return function (nextPoint, candidate) {
      if (nextPoint === currentPoint || isLeftOf({ a: currentPoint, b: nextPoint }, candidate)) {
        return candidate;
      }
      return nextPoint;
  }
}

function isLeftOf({ a, b }, p) {
  return (b.x - a.x) * (p.y - a.y) > (p.x - a.x) * (b.y - a.y);
}

const points = [
  { x: -5, y: 2 },
  { x: 5, y: 7 },
  { x: -6, y: -12 },
  { x: -14, y: -14 },
  { x: 9, y: 9 },
  { x: -1, y: -1 },
  { x: -10, y: 11 },
  { x: -6, y: 15 },
  { x: -6, y: -8 },
  { x: 15, y: -9 },
  { x: 7, y: -7 },
  { x: -2, y: -9 },
  { x: 6, y: -5 },
  { x: 0, y: 14 },
  { x: 2, y: 8 }
];

const convexHull = jarvisMarch(points);
convexHull.forEach(p => console.log(`(${p.x}, ${p.y})`));
