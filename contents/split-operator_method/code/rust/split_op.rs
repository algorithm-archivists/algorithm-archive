extern crate rustfft;

use rustfft::num_complex::Complex;
use rustfft::FFTplanner;
use std::f64::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

// This implementation is based on the C and C++ implementations.

#[derive(Clone)]
struct Parameters {
    xmax: f64,
    res: usize,
    dt: f64,
    timesteps: usize,
    dx: f64,
    x: Vec<f64>,
    dk: f64,
    k: Vec<f64>,
    im_time: bool,
}

impl Parameters {
    pub fn new(xmax: f64, res: usize, dt: f64, timesteps: usize, im_time: bool) -> Parameters {
        let dx = 2.0_f64 * xmax / (res as f64);
        let mut x: Vec<f64> = Vec::with_capacity(res);
        let dk = PI / xmax;
        let mut k: Vec<f64> = Vec::with_capacity(res);
        for i in 0..res {
            x.push(xmax / (res as f64) - xmax + (i as f64) * dx);
            match i {
                i if (i < res / 2) => k.push((i as f64) * PI / xmax),
                _ => k.push(((i as f64) - (res as f64)) * PI / xmax),
            }
        }
        Parameters {
            xmax,
            res,
            dt,
            timesteps,
            im_time,
            dx,
            x,
            dk,
            k,
        }
    }
}

struct Operators {
    v: Vec<Complex<f64>>,
    pe: Vec<Complex<f64>>,
    ke: Vec<Complex<f64>>,
    wfc: Vec<Complex<f64>>,
}

impl Operators {
    pub fn new(par: &Parameters, v_offset: f64, wfc_offset: f64) -> Operators {
        let mut v: Vec<Complex<f64>> = Vec::with_capacity(par.res);
        let mut pe: Vec<Complex<f64>> = Vec::with_capacity(par.res);
        let mut ke: Vec<Complex<f64>> = Vec::with_capacity(par.res);
        let mut wfc: Vec<Complex<f64>> = Vec::with_capacity(par.res);

        for i in 0..par.res {
            v.push(Complex::new(
                0.5_f64 * (par.x[i] - v_offset).powi(2),
                0.0_f64,
            ));
            wfc.push(Complex::new(
                (-((par.x[i] - wfc_offset).powi(2)) / 2.0_f64).exp(),
                0.0_f64,
            ));
            if par.im_time {
                ke.push(Complex::new(
                    (-0.5_f64 * par.dt * par.k[i].powi(2)).exp(),
                    0.0_f64,
                ));
                pe.push(Complex::new((-0.5_f64 * par.dt * v[i].re).exp(), 0.0_f64));
            } else {
                ke.push(Complex::new(
                    0.0_f64,
                    (-0.5_f64 * par.dt * par.k[i].powi(2)).exp(),
                ));
                pe.push(Complex::new(0.0_f64, (-0.5_f64 * par.dt * v[i].re).exp()));
            }
        }
        Operators { v, pe, ke, wfc }
    }
}

fn fft(x: &mut Vec<Complex<f64>>, inverse: bool) {
    let mut y = vec![Complex::new(0.0_f64, 0.0_f64); x.len()];
    let mut p = FFTplanner::new(inverse);
    let fft = p.plan_fft(x.len());
    fft.process(x.as_mut_slice(), y.as_mut_slice());

    for i in 0..x.len() {
        x[i] = y[i] / (x.len() as f64).sqrt();
    }
}

fn split_op(par: &Parameters, opr: &mut Operators) {
    let mut density: Vec<f64>;

    for i in 0..par.timesteps {
        for j in 0..par.res {
            opr.wfc[j] *= opr.pe[j];
        }

        fft(&mut opr.wfc, false);

        for j in 0..par.res {
            opr.wfc[j] *= opr.ke[j];
        }

        fft(&mut opr.wfc, true);

        for j in 0..par.res {
            opr.wfc[j] *= opr.pe[j];
        }

        density = opr.wfc.iter().map(|x| x.norm().powi(2)).collect();

        if par.im_time {
            let sum = density.iter().sum::<f64>() * par.dx;

            for j in 0..par.res {
                opr.wfc[j] /= sum.sqrt();
            }
        }

        // Writing data into a file in the format of:
        // index, density, real potential.
        let path_name = format!("output{}.dat", i);
        let path = Path::new(&path_name);
        let display = path.display();

        let mut file = match File::create(&path) {
            Err(why) => panic!("Couldn't create {}: {}", display, why),
            Ok(good) => good,
        };

        for j in 0..par.res {
            if let Err(why) = writeln!(file, "{}\t{}\t{}", j, density[j], opr.v[j].re) {
                panic!("Couldn't write to {}: {}", display, why)
            }
            if let Err(why) = file.flush() {
                panic!("Couldn't flush {}: {}", display, why)
            }
        }
    }
}

fn calculate_energy(par: &Parameters, opr: &Operators) -> f64 {
    let wfc_r = opr.wfc.clone();
    let mut wfc_k = opr.wfc.clone();
    let mut wfc_c = vec![Complex::new(0.0_f64, 0.0_f64); par.res];

    fft(&mut wfc_k, false);

    for i in 0..par.res {
        wfc_c[i] = wfc_r[i].conj();
    }

    let mut energy_k = vec![Complex::new(0.0_f64, 0.0_f64); par.res];
    let mut energy_r = vec![Complex::new(0.0_f64, 0.0_f64); par.res];

    for i in 0..par.res {
        energy_k[i] = wfc_k[i] * Complex::new(par.k[i], 0.0_f64).powi(2);
    }

    fft(&mut energy_k, true);

    for i in 0..par.res {
        energy_k[i] *= wfc_c[i].scale(0.5_f64);
        energy_r[i] = wfc_c[i] * opr.v[i] * wfc_r[i];
    }

    let energy_final = energy_k
        .into_iter()
        .zip(energy_r.into_iter())
        .fold(0.0_f64, |acc, x| acc + (x.0 + x.1).re);

    energy_final * par.dx
}

fn main() {
    let par = Parameters::new(5.0, 256, 0.05, 100, true);
    let mut opr = Operators::new(&par, 0.0, -1.0);

    split_op(&par, &mut opr);

    println!("The energy is {}", calculate_energy(&par, &opr));
}
