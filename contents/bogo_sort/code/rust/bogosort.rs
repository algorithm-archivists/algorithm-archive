// Submitted by jess 3jane

extern crate rand;

use rand::{thread_rng, Rng};

fn is_sorted(arr: &[i32]) -> bool {
    for i in 1..arr.len() {
        if arr[i - 1] > arr[i] {
            return false;
        }
    }
    true
}

fn bogo_sort(arr: &mut [i32]) {
    while !is_sorted(arr) {
        thread_rng().shuffle(arr);
    }
}

fn main() {
    let mut v = vec![1, 2, 3, 4, 5];
    thread_rng().shuffle(&mut v);
    println!("Original array: {:?}", v);
    bogo_sort(&mut v);
    println!("Sorted array: {:?}", v);
}
