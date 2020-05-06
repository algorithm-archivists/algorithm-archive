extern crate num;
extern crate rand;

use num::complex::Complex;
use rand::prelude::*;
use std::f64::consts::PI;

// This is based on the Python implementation.

fn dft(x: &[Complex<f64>]) -> Vec<Complex<f64>> {
    let n = x.len();
    (0..n)
        .map(|i| {
            (0..n)
                .map(|k| {
                    x[k] * (Complex::new(0.0_f64, -2.0_f64) * PI * (i as f64) * (k as f64)
                        / (n as f64))
                        .exp()
                })
                .sum()
        })
        .collect::<Vec<_>>()
}

fn cooley_tukey(x: &[Complex<f64>]) -> Vec<Complex<f64>> {
    let n = x.len();
    if n <= 1 {
        return x.to_owned();
    }
    let even = cooley_tukey(&x.iter().step_by(2).cloned().collect::<Vec<_>>());
    let odd = cooley_tukey(&x.iter().skip(1).step_by(2).cloned().collect::<Vec<_>>());

    let mut temp = vec![Complex::new(0.0_f64, 0.0_f64); n];
    for k in 0..(n / 2) {
        temp[k] = even[k]
            + (Complex::new(0.0_f64, -2.0_f64) * PI * (k as f64) / (n as f64)).exp() * odd[k];
        temp[k + n / 2] = even[k]
            - (Complex::new(0.0_f64, -2.0_f64) * PI * (k as f64) / (n as f64)).exp() * odd[k];
    }
    temp
}

fn bit_reverse(x: &[Complex<f64>]) -> Vec<Complex<f64>> {
    let n = x.len();
    let mut temp = vec![Complex::new(0.0_f64, 0.0_f64); n];
    for k in 0..n {
        let b: usize = (0..((n as f64).log2() as usize))
            .filter(|i| k >> i & 1 != 0)
            .map(|i| 1 << ((((n as f64).log2()) as usize) - 1 - i))
            .sum();
        temp[k] = x[b];
        temp[b] = x[k];
    }
    temp
}

fn iterative_cooley_tukey(x: &[Complex<f64>]) -> Vec<Complex<f64>> {
    let n = x.len();

    let mut new_x = bit_reverse(x);

    for i in 1..=((n as f64).log2() as usize) {
        let stride = 2_u128.pow(i as u32);
        let w = (Complex::new(0.0_f64, -2.0_f64) * PI / (stride as f64)).exp();
        for j in (0..n).step_by(stride as usize) {
            let mut v = Complex::new(1.0_f64, 0.0_f64);
            for k in 0..((stride / 2) as usize) {
                let k_plus_j = new_x[k + j];
                let k_plus_j_plus_half_stride = new_x[k + j + ((stride / 2) as usize)];
                new_x[k + j + ((stride / 2) as usize)] = k_plus_j - v * k_plus_j_plus_half_stride;
                let k_plus_j_plus_half_stride_2 = new_x[k + j + ((stride / 2) as usize)];
                new_x[k + j] -= k_plus_j_plus_half_stride_2 - k_plus_j;
                v *= w;
            }
        }
    }

    new_x
}

fn main() {
    let mut x = Vec::with_capacity(64);
    let mut rng = thread_rng();
    for _i in 0..64 {
        let real = rng.gen_range(0.0_f64, 1.0_f64);
        x.push(Complex::new(real, 0.0_f64));
    }
    let y = cooley_tukey(&x.clone());
    let z = iterative_cooley_tukey(&x.clone());
    let t = dft(&x.clone());

    println!(
        "{}",
        y.iter().zip(z.iter()).all(|i| (i.0 - i.1).norm() < 1.0)
    );
    println!(
        "{}",
        y.iter()
            .zip(t.into_iter())
            .all(|i| (i.0 - i.1).norm() < 1.0)
    );
}
