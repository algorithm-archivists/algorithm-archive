use std::fs::File;
use std::io::Write;
use std::cmp::max;

fn main() {
    let x = normalize(create_sawtooth(200));
    let y = normalize(create_sawtooth(200));

    let len_x = x.len();
    let len_y = y.len();

    let full_linear_output = convolve_linear(&x, &y, len_x + len_y - 1);
    let simple_linear_output = convolve_linear(&x, &y, len_x);
    let cyclic_output = convolve_cyclic(&x, &y);

    // Save the convolutions to plot them.
    // The way I do it is a little weird but it is to store the data the same way as the other programs
    let mut full_file = File::create("full_linear.dat").unwrap();
    for i in full_linear_output {
        write!(full_file, "{}\n", i).unwrap();
    }
    full_file.sync_all().unwrap();

    let mut simple_file = File::create("simple_linear.dat").unwrap();
    for i in simple_linear_output {
        write!(simple_file, "{}\n", i).unwrap();
    }
    simple_file.sync_all().unwrap();

    let mut cyclic_file = File::create("cyclic.dat").unwrap();
    for i in cyclic_output {
        write!(cyclic_file, "{}\n", i).unwrap();
    }
    cyclic_file.sync_all().unwrap();
}

// Generates a sawtooth function with a given length.
fn create_sawtooth(length: usize) -> Vec<f64> {
    let mut array: Vec<f64> = Vec::with_capacity(length);
    for i in 0..length {
        array.push((i+1) as f64 / 200.);
    }
    array
}

// Normalizes the given array.
fn normalize(array: Vec<f64>) -> Vec<f64> {
    let norm = norm(&array);
    let mut output: Vec<f64> = Vec::with_capacity(array.len());

    for i in 0..array.len() {
        output.push(array[i] / norm);
    }

    output
}

// Calculates the norm of an array.
fn norm(array: &Vec<f64>) -> f64 {
    let sum = array.iter().map(|i| i * i).sum::<f64>()
    sum.sqrt()
}


// Modulus function that handles negative values correctly.
// Assumes that y >= 0.
fn modulus(x: isize, y: isize) -> isize {((x%y) + y) % y}

fn convolve_linear(signal: &Vec<f64>, filter: &Vec<f64>, output_size: usize) -> Vec<f64> {
    let mut output = Vec::with_capacity(output_size);

    for i in 0..(output_size as isize) {
        let mut sum: f64 = 0.;
        for j in max(0, i as isize - filter.len() as isize)..=i as isize {
            if j < signal.len() as isize && (i - j) < filter.len() as isize {
                sum += signal[j as usize] * filter[(i-j) as usize]
            }
        }
        output.push(sum);
    }

    output
}

fn convolve_cyclic(signal: &Vec<f64>, filter: &Vec<f64>) -> Vec<f64> {
    let output_size = max(signal.len(), filter.len()) as isize;

    let mut output: Vec<f64> = Vec::with_capacity(output_size as usize);
    for i in 0..output_size {
        let mut sum: f64 = 0.;
        for j in 0..output_size {
            if modulus(i - j, output_size) < filter.len() as isize {
                sum += signal[modulus(j - 1, output_size) as usize] * filter[modulus(i - j, output_size) as usize];
            }
        }
        output.push(sum);
    }
    output
}
