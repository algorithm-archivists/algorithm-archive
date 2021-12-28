use rand::*;

#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

fn chaos_game(iters: usize, shapes: Vec<Point>) -> Vec<Point> {
    let mut rng = rand::thread_rng();
    let mut p   = Point{x: rng.gen(), y: rng.gen()};

    (0..iters).into_iter().map(|_| {
        let old_point = p;
        let tmp = shapes[rng.gen_range(0..shapes.len())];
        p.x = 0.5 * (p.x + tmp.x);
        p.y = 0.5 * (p.y + tmp.y);
        old_point
    }).collect()
}

fn main() {
    let shapes = vec![
        Point{x: 0., y: 0.},
        Point{x: 0.5, y: 0.75_f64.sqrt()},
        Point{x: 1., y: 0.},
    ];

    let mut out = String::new();

    for point in chaos_game(10_000, shapes) {
        out += format!("{}\t{}\n", point.x, point.y).as_str();
    }

    std::fs::write("./sierpinski.dat", out).unwrap();
}