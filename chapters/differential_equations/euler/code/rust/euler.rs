fn main() {
    let mut result = [0.0;100];
    let threshold = 0.01;
    let timestep = 0.01;

    solve_euler(timestep, &mut result, 100);
    println!("{}", check_result(&result, 100, threshold, timestep));
}


fn solve_euler(timestep: f64, result: &mut [f64], n: usize) {
    if n != 0 {
        result[0] = 1.0;
        for i in 1..n {
            result[i] = result[i-1] - 3.0 * result[i-1] * timestep;
        }
    }
}


fn check_result(result: &[f64],  n: usize,  threshold: f64,  timestep: f64) -> bool {
    let mut is_approx: bool = true;
    for i in 0..n {
        let solution = (-3.0 * i as f64 * timestep).exp();
        if (result[i] - solution).abs() > threshold {
            println!("{}    {}", result[i], solution);
            is_approx = false;
        }
    }

    return is_approx;
}

