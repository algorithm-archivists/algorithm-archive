
type Point = (i64, i64);

// Is the turn counter clockwise?
fn turn_counter_clockwise(p1: Point, p2: Point, p3: Point) -> bool {
    (p3.1 - p1.1) * (p2.0 - p1.0) >= (p2.1 - p1.1) * (p3.0 - p1.0)
}

fn jarvis_march(gift: &[Point]) -> Option<Vec<Point>> {
    // There can only be a convex hull if there are more than 2 points
    if gift.len() < 3 {
        return None;
    }

    let leftmost_point = gift
        // Iterate over all points
        .iter()
        // Find the point with minimum x
        .min_by_key(|i| i.0)
        // If there are no points in the gift, there might
        // not be a minimum. Unwrap fails (panics) the program
        // if there wasn't a minimum, but we know there always
        // is because we checked the size of the gift.
        .unwrap()
        .clone();

    let mut hull = vec![leftmost_point];

    let mut point_on_hull = leftmost_point;
    loop {
        // Search for the next point on the hull
        let mut endpoint = gift[0];
        for i in 1..gift.len() {
            if endpoint == point_on_hull || !turn_counter_clockwise(gift[i], hull[hull.len() - 1], endpoint) {
                endpoint = gift[i];
            }
        }

        point_on_hull = endpoint;

        // Stop whenever we got back to the same point
        // as we started with, and we wrapped the gift
        // completely.
        if hull[0] == endpoint {
            break;
        } else {
            hull.push(point_on_hull);
        }
    }

    Some(hull)
}

fn main() {
    let test_gift = vec![
        (-5, 2), (5, 7), (-6, -12), (-14, -14), (9, 9),
        (-1, -1), (-10, 11), (-6, 15), (-6, -8), (15, -9),
        (7, -7), (-2, -9), (6, -5), (0, 14), (2, 8)
    ];

    let hull = jarvis_march(&test_gift);

    println!("The points in the hull are: {:?}", hull);
}
