use std::fs::File;
use std::io::Write;

// require create rand:
use rand::prelude::SliceRandom;
use rand::Rng;

type Point = (f64, f64);

// This function simulates a "chaos game"
fn chaos_game(n: u32, shape_points: Vec<Point>) -> Vec<Point> {
    let mut rng = rand::thread_rng();

    // initialize the output vector and the initial point
    let mut output_points: Vec<Point> = Vec::new();
    let mut point = (rng.gen(), rng.gen());

    for _ in 0..n {
        output_points.push(point);

        let tmp = shape_points
            .choose(&mut rng)
            .expect("could not choose a shape point");

        point = (0.5 * (point.0 + tmp.0), 0.5 * (point.1 + tmp.1));
    }

    output_points
}

fn main() {

    // This will generate a Sierpinski triangle with a chaos game of n points for an
    // initial triangle with three points on the vertices of an equilateral triangle:
    //     A = (0.0, 0.0)
    //     B = (0.5, sqrt(0.75))
    //     C = (1.0, 0.0)
    // It will output the file sierpinski.dat, which can be plotted after
    
    let shape_points = vec![(0.0, 0.0), (0.5, 0.75_f64.sqrt()), (1.0, 0.0)];

    let output_points = chaos_game(10000, shape_points);

    let mut file = File::create("sierpinski.dat").expect("Unable to open/create the file");

    for (x, y) in output_points {
        writeln!(&mut file, "{}\t{}", x, y).expect("Unable to write to file");
    }
}
