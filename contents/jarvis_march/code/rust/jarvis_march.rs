type Point = (i32, i32);

/// Checks if point B is counter-clockwise of point C relative to point A
///
/// This is a very simple cross product of the vectors ab and ac
/// but we've replaced the normal subtraction with a comparison
/// which tells us if the result would have been negative (i.e.
/// the angle is counter clockwise)
fn ccw(a: &Point, b: &Point, c: &Point) -> bool {
    (c.1 - a.1) * (b.0 - a.0)
        >= (b.1 - a.1) * (c.0 - a.0)
}

fn jarvis_march<'a>(points: &'a [Point]) -> Vec<&'a Point> {
    if points.len() == 0 { return Vec::new(); }

    // The most negative point *must* be on the hull so
    // we use it as a starting point
    let mut hull = vec![points.iter().min().unwrap()];

    loop {
        let last_point = hull.last().unwrap();

        // Loop over possible points and find the one furthest to the left of the 
        // last point added on the hull
        let mut endpoint = &points[0];
        for i in 1..points.len() {
            // In this case when we say "left" we really mean the point which is 
            // counter clockwise of all other points relative to the last point
            // on the hull
            if endpoint == *last_point || !ccw(&points[i], last_point, endpoint) {
                endpoint = &points[i];
            }
        }

        // If we've looped back around to the first point in the hull the hull is now closed
        if hull[0] == endpoint {
            return hull;
        } else {
            // Otherwise push this point onto the hull and keep going
            hull.push(endpoint);
        }
    }
}

fn main() {
    let points = [
        ( -5,   2), (  5,   7), ( -6, -12),
        (-14, -14), (  9,   9), ( -1,  -1),
        (-10,  11), ( -6,  15), ( -6,  -8),
        ( 15,  -9), (  7,  -7), ( -2,  -9),
        (  6,  -5), (  0,  14), (  2,   8),
    ];
    let hull = jarvis_march(&points);

    println!("Hull points are: ");
    for (x, y) in hull.iter() {
        println!("({}, {})", x, y);
    }
}
