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
  { x: 1, y: 3 },
  { x: 2, y: 4 },
  { x: 4, y: 0 },
  { x: 1, y: 0 },
  { x: 0, y: 2 },
  { x: 2, y: 2 },
  { x: 3, y: 4 },
  { x: 3, y: 1 },
];

const convexHull = jarvisMarch(points);
convexHull.forEach(p => console.log(`(${p.x}, ${p.y})`));

