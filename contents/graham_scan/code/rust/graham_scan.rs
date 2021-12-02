use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
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

// Check if the turn of the points is counter clockwise.
fn counter_clockwise(a: &Point, b: &Point, c: &Point) -> bool {
    (b.x - a.x) * (c.y - a.y) >= (b.y - a.y) * (c.x - a.x)
}

// Calculate the polar angle of a  point relative to a reference point.
fn polar_angle(reference: &Point, point: &Point) -> f64 {
    (point.y - point.y).atan2(point.x - reference.x)
}

fn graham_scan(mut points: Vec<Point>) -> Vec<Point> {
    // First, sort the points so the one with the lowest y-coordinate comes first (the pivot)
    points.sort_unstable();

    // Take ownership of the pivot point
    let pivot = points.remove(0);

    // Sort all points based on the angle between the pivot point and itself
    &mut points
        .sort_by(|a, b| (polar_angle(a, &pivot).partial_cmp(&polar_angle(b, &pivot))).unwrap());

    // Reinsert the pivot point
    points.insert(0, pivot);

    let n = points.len();
    let mut m = 1;

    // Move the points of the hull towards the beginning of the vector.
    for mut i in 2..n {
        while counter_clockwise(&points[m - 1], &points[m], &points[i]) {
            if m > 1 {
                m -= 1;
            } else if m == i {
                break;
            } else {
                i += 1;
            }
        }

        m += 1;
        points.swap(i, m);
    }

    // Remove all non-hull points from the vector
    points.truncate(m + 1);
    points
}

fn main() {
    let points = vec![
        Point { x: 1.0, y: 3.0 },
        Point { x: 2.0, y: 4.0 },
        Point { x: 4.0, y: 0.0 },
        Point { x: 1.0, y: 0.0 },
        Point { x: 0.0, y: 2.0 },
        Point { x: 2.0, y: 2.0 },
        Point { x: 3.0, y: 4.0 },
        Point { x: 3.0, y: 1.0 },
    ];

    let hull_points = graham_scan(points);
    println!("{:#?}", hull_points);
}
