fn thomas(a: &[f64], b: &[f64], c: &[f64], x: &[f64]) -> Vec<f64> {
    let size = a.len();
    let mut y = vec![0.0; size];
    let mut z = Vec::from(x);

    y[0] = c[0] / b[0];
    z[0] = x[0] / b[0];

    for i in 1..size {
        let scale = 1.0 / (b[i] - a[i] * y[i - 1]);
        y[i] = c[i] * scale;
        z[i] = (z[i] - a[i] * z[i - 1]) * scale;
    }

    for i in (0..(size - 1)).rev() {
        z[i] -= y[i] * z[i + 1];
    }

    z
}

fn main() {
    let a = vec![0.0, 2.0, 3.0];
    let b = vec![1.0, 3.0, 6.0];
    let c = vec![4.0, 5.0, 0.0];
    let x = vec![7.0, 5.0, 3.0];

    println!("The system");
    println!("[{:?} {:?} {:?}][x] = [{:?}]", a[0], b[0], c[0], &x[0]);
    println!("[{:?} {:?} {:?}][x] = [{:?}]", a[1], b[1], c[1], &x[1]);
    println!("[{:?} {:?} {:?}][x] = [{:?}]", a[2], b[2], c[2], &x[2]);
    println!("has the solution");

    let y = thomas(&a, &b, &c, &x);

    y.iter()
        .for_each(|i| println!("[{:>19}]", format!("{:18}", format!("{:?}", i))));
}
