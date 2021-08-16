// Submitted by jess 3jane

extern crate rand;

use std::f64::consts::PI;

fn in_circle(x: f64, y: f64, radius: f64) -> bool {
    x * x + y * y < radius * radius
}

fn monte_carlo(n: i64) -> f64 {
    let mut count = 0;

    for _ in 0..n {
        let x = rand::random();
        let y = rand::random();
        if in_circle(x, y, 1.0) {
            count += 1;
        }
    }

    // return our pi estimate
    (4 * count) as f64 / n as f64
}

fn main() {
    let pi_estimate = monte_carlo(10000000);

    println!(
        "Percent error is {:.3}%",
        (100.0 * (pi_estimate - PI).abs() / PI)
    );
}
