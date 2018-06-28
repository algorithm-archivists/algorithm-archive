// Submitted by jess 3jane

extern crate rand;

fn shuffle(arr : &mut [i32]) {
    for i in 0..arr.len() {
        let r : usize = rand::random::<usize>() % arr.len();
        arr.swap(r,i);
    }
}

fn is_sorted(arr : &[i32]) -> bool {
    for i in 1..arr.len() {
        if arr[i-1] > arr[i] {
            return false;
        }
    }
    true
}

fn bogo_sort(arr : &mut [i32]) {
    while !is_sorted(arr) {
        shuffle(arr)
    }
}

fn main() {
    let mut v = vec!(1,2,3,4,5);
    shuffle(&mut v);
    println!("Original array: {:?}", v);
    bogo_sort(&mut v);
    println!("Sorted array: {:?}", v);
}
