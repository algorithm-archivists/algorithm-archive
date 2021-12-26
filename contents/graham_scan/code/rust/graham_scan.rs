use std::cmp::Ordering;

#[derive(Debug, PartialEq, Copy, Clone)]
struct Point {
    x: f64,
    y: f64,
}

impl Eq for Point {}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.y == other.y {
            self.x.partial_cmp(&other.x)
        } else {
            self.y.partial_cmp(&other.y)
        }
    }
}

// Defines an order for Points so they can be sorted
impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        // Neither field of Point will be NaN, so this is safe
        self.partial_cmp(other).unwrap()
    }
}

// Determines whether the angle abc is clockwise, counter-clockwise or colinear
// result > 0 : counter-clockwise
// result = 0 : colinear
// result < 0 : clockwise
fn counter_clockwise(a: &Point, b: &Point, c: &Point) -> f64 {
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

// Calculate the polar angle of a  point relative to a reference point.
fn polar_angle(reference: &Point, point: &Point) -> f64 {
    (point.y - reference.y).atan2(point.x - reference.x)
}

fn graham_scan(mut points: Vec<Point>) -> Vec<Point> {
    if points.is_empty() {
        return Vec::new();
    }

    // Unwrap is safe because length is > 0
    let start = *points.iter().min().unwrap();
    points.retain(|a| a != &start);
    points.sort_unstable_by(|a, b| polar_angle(&start, a).partial_cmp(&polar_angle(&start, b)).unwrap());

    let mut hull: Vec<Point> = vec![start, points[0], points[1]];

    for pt in points[2..points.len()].iter() {
        while counter_clockwise(&hull[hull.len() - 2], &hull[hull.len() - 1], pt) < 0.0 {
            hull.pop();
        }
        hull.push(*pt);
    }
    hull
}

fn main() {
    let points = vec![
        Point { x:  -5.0, y:   2.0 },
        Point { x:   5.0, y:   7.0 },
        Point { x:  -6.0, y: -12.0 },
        Point { x: -14.0, y: -14.0 },
        Point { x:   9.0, y:   9.0 },
        Point { x:  -1.0, y:  -1.0 },
        Point { x: -10.0, y:  11.0 },
        Point { x:  -6.0, y:  15.0 },
        Point { x:  -6.0, y:  -8.0 },
        Point { x:  15.0, y:  -9.0 },
        Point { x:   7.0, y:  -7.0 },
        Point { x:  -2.0, y:  -9.0 },
        Point { x:   6.0, y:  -5.0 },
        Point { x:   0.0, y:  14.0 },
        Point { x:   2.0, y:   8.0 },
    ];

    let hull_points = graham_scan(points);
    println!("{:#?}", hull_points);
}
