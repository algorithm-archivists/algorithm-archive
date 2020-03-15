type Point = (i32, i32);

fn ccw(p1: &Point, p2: &Point, p3: &Point) -> bool {
    (p3.1 - p1.1) * (p2.0 - p1.0)
        >= (p2.1 - p1.1) * (p3.0 - p1.0)
}

fn jarvis_march<'a>(points: &'a [Point]) -> Vec<&'a Point> {
    if points.len() == 0 { return Vec::new(); }

    let mut hull = vec![points.iter().min().unwrap()];

    loop {
        let last_point = hull.last().unwrap();

        let mut endpoint = &points[0];
        for i in 1..points.len() {
            if endpoint == *last_point || !ccw(&points[i], last_point, endpoint) {
                endpoint = &points[i];
            }
        }

        if hull[0] == endpoint {
            return hull;
        } else {
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
