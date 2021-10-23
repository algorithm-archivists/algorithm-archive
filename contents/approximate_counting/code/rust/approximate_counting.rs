// This function takes
//     - v: value in register
//     - a: a  scaling value for the logarithm based on Morris's paper
// It returns n(v,a), the approximate count
fn n(v: f64, a: f64) -> f64 {
    a * ((1_f64 + 1_f64 / a).powf(v) - 1_f64)
}


// This function takes
//    - v: value in register
//    - a: a scaling value for the logarithm based on Morris's paper
// It returns a new value for v
fn increment(v: f64, a: f64) -> f64 {
    // delta is the probability of incrementing our counter
    let delta = 1_f64 / (n(v + 1_f64, a) - n(v, a));

    if rand::random::<f64>() <= delta {
        v + 1_f64
    } else {
        v
    }
}

// This simulates counting and takes
//     - n_items: number of items to count and loop over
//     - a: a scaling value for the logarithm based on Morris's paper
// It returns n(v,a), the approximate count
fn approximate_count(n_items: usize, a: f64) -> f64 {
    let mut v = 0_f64;

    for _ in 0..n_items {
        v = increment(v, a);
    }

    v
}

// This function takes
//     - n_trials: the number of counting trials
//     - n_items: the number of items to count to
//     - a: a scaling value for the logarithm based on Morris's paper
//     - threshold: the maximum percent error allowed
// It returns a "passed" / "failed" test value
fn test_approximate_count(n_trials: usize, n_items: usize, a: f64, threshold: f64) {
    let avg = std::iter::from_fn(|| Some(approximate_count(n_items, a)))
                        .take(n_trials)
                        .sum::<f64>() / n_trials as f64;
    
    let n_items_float = n_items as f64;
    
    if ((avg - n_items_float) / n_items_float) < threshold {
        println!("passed");
    } else {
        println!("failed");
    }
    
}

fn main() {
    println!("testing 1,000, a = 30, 10% error");
    test_approximate_count(100, 1000, 30_f64, 0.1);
    println!("testing 12,345, a = 10, 10% error");
    test_approximate_count(100, 12345, 10_f64, 0.1);
    println!("testing 222,222, a = 0.5, 20% error");
    test_approximate_count(100, 222222, 0.5, 0.2);
}
