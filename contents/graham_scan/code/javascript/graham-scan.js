function grahamScan(points) {
  // First, sort the points so the one with the lowest y-coordinate comes first (the pivot)
  points = [...points].sort((a, b) => (a.y - b.y));
  const pivot = points[0];

  // Then sort all remaining points based on the angle between the pivot and itself
  const hull = points.slice(1).sort((a, b) => polarAngle(a, pivot) - polarAngle(b, pivot));

  // The pivot is always on the hull
  hull.unshift(pivot);

  let n = hull.length;
  let m = 1;
  for (let i = 2; i < n; i++) {
    while (ccw(hull[m - 1], hull[m], hull[i]) <= 0) {
      if (m > 1) {
        m -= 1;
      } else if (m === i) {
        break;
      } else {
        i += 1;
      }
    }

    m += 1;
    [hull[i], hull[m]] = [hull[m], hull[i]];
  }

  return hull.slice(0, m + 1);
}

function polarAngle(a, b) {
  return Math.atan2(a.y - b.y, a.x - b.x);
}

function ccw(a, b, c) {
  return (b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y);
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
  { x: 2, y: 8 },
];

const convexHull = grahamScan(points);
console.log("The points in the hull are:");
convexHull.forEach(p => console.log(`(${p.x}, ${p.y})`));
