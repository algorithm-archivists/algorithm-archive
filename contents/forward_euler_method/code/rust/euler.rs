fn main() {
    let mut result = [0.0; 100];
    let threshold = 0.01;
    let timestep = 0.01;

    solve_euler(timestep, &mut result);
    println!("{}", check_result(&result, threshold, timestep));
}

fn solve_euler(timestep: f64, result: &mut [f64]) {
    let n = result.len();
    if n != 0 {
        result[0] = 1.0;
        for i in 1..n {
            result[i] = result[i - 1] - 3.0 * result[i - 1] * timestep;
        }
    }
}

fn check_result(result: &[f64], threshold: f64, timestep: f64) -> bool {
    let mut is_approx: bool = true;
    for (i, val) in result.iter().enumerate() {
        let solution = (-3.0 * i as f64 * timestep).exp();
        if (val - solution).abs() > threshold {
            println!("{}    {}", val, solution);
            is_approx = false;
        }
    }

    return is_approx;
}
