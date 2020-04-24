extern crate rand; // External crate that provides random number generation tools

use rand::distributions::Uniform; // Used for a uniform distribution
use rand::{thread_rng, Rng}; // Used for random number generation

fn bubble_sort(a: &mut [u32]) {
    let n = a.len();

    for _ in 0..n {
        for j in 1..n {
            if a[j - 1] > a[j] {
                a.swap(j, j - 1);
            }
        }
    }
}

fn main() {
    let mut rng = thread_rng(); // Create random number generator
    let num_range = Uniform::new_inclusive(0, 10000); // Obtain uniform distribution of range [0, 10000]
    let mut rand_vec: Vec<u32> = rng.sample_iter(&num_range).take(10).collect();
    // Generates random values over that range, take 10 values from it and collect in vector

    println!("Before sorting: {:?}", rand_vec);
    bubble_sort(&mut rand_vec);
    println!("After sorting: {:?}", rand_vec);
}
