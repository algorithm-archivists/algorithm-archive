fn n(v: f64, a: f64) -> f64 {
    a * ((1_f64 + 1_f64 / a).powf(v) - 1_f64)
}

fn increment(v: f64, a: f64) -> f64 {
    let delta = 1_f64 / (n(v + 1_f64, a) - n(v, a));

    if rand::random::<f64>() <= delta {
        v + 1_f64
    } else {
        v
    }
}

fn approximate_count(n_items: usize, a: f64) -> f64 {
    let mut v = 0_f64;

    for _ in 0..n_items {
        v = increment(v, a);
    }

    v
}

fn test_approximate_count(n_trails: usize, n_items: usize, a: f64, threshold: f64) {
    //let avg = std::iter::from_fn(|| Some(approximate_count(n_items, a)))
    //                    .take(n_trails)
    //                    .sum::<f64>() / n_trails as f64;
    
    let mut avg = 0_f64;
    for _ in 0..n_trails {
    	avg += approximate_count(n_items, a);
    }
    
    avg /= n_trails as f64;
    
    let n_items_float = n_items as f64;
    
    if ((avg - n_items_float) / n_items_float) < threshold {
        println!("pass");
    } 
    
}

fn main() {
    println!("testing 1,000, a = 30, 1% error");
    test_approximate_count(100, 1000, 30_f64, 0.1);
    println!("testing 12,345, a = 10, 1% error");
    test_approximate_count(100, 12345, 10_f64, 0.1);
    println!("testing 222,222, a = 0.5, 10% error");
    test_approximate_count(100, 222222, 0.5, 0.2);
}