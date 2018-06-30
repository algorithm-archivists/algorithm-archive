// submitted by jess 3jane

use std::cmp::min;

pub struct Matrix {
    rows: usize,
    cols: usize,
    data: Vec<f64>,
}

impl Matrix {
    fn new(rows: usize, cols: usize) -> Matrix {
        let mut data = Vec::with_capacity(rows*cols);
        for _ in 0..rows*cols { data.push(0.0); }
        Matrix { rows, cols, data }
    }

    fn get(&self, row: usize, col: usize) -> f64 {
        self.data[row * self.cols + col]
    }

    fn set(&mut self, row: usize, col: usize, value: f64) {
        self.data[row * self.cols + col] = value;
    }

    fn swap_rows(&mut self, a: usize, b: usize) {
        for col in 0..self.cols {
            self.data.swap(a * self.cols + col, b * self.cols + col);
        }
    }
}

fn gaussian_elimination(a: &mut Matrix) {
    for k in 0..min(a.cols, a.rows) {
        // Step 1: find the maximum element for this kumn
        let mut max_row = 0;
        let mut max_value = 0.0;
        for row in k..a.rows {
            if max_value < a.get(row, k).abs() {
                max_value = a.get(row, k).abs();
                max_row = row;
            }
        }

        // Check to make sure the matrix is good
        if a.get(max_row, k) == 0.0 {
            println!("Matrix is singular, aborting");
            return;
        }

        // Step 2: swap the row with the highest value for this kumn to the top
        a.swap_rows(k, max_row);

        // Loop over all remaining rows
        for i in k+1..a.rows {
            // Step 3: find the fraction
            let fraction = a.get(i, k)/a.get(k, k);

            // Loop through all columns for that row
            for j in (k+1)..a.cols {
                // Step 4: re-evaluate each element
                let val = a.get(i, j) - a.get(k, j)*fraction;
                a.set(i, j, val);
            }

            // Step 5: set lower elements to 0 
            a.set(i, k, 0.0);
        }
    }
}

fn back_substitution(a: &Matrix) -> Vec<f64> {
    let mut soln = Vec::with_capacity(a.rows);
    for _ in 0..a.rows { soln.push(0.0); }
    
    soln[a.rows - 1] = a.get(a.rows - 1, a.cols - 1) / a.get(a.rows - 1, a.cols - 2);

    for i in (0..a.rows - 1).rev() {
        let mut sum = 0.0;
        for j in (i..a.rows).rev() {
            sum += soln[j] * a.get(i, j);
        }
        soln[i] = (a.get(i, a.cols - 1) - sum) / a.get(i,i);
    }

    soln
}

fn main() {
    // The example matrix from the text
    let mut a = Matrix::new(3,4);
    a.data = vec![2.0,  3.0, 4.0,  6.0,
                  1.0,  2.0, 3.0,  4.0,
                  3.0, -4.0, 0.0, 10.0,];
    
    gaussian_elimination(&mut a);
    let soln = back_substitution(&a);
    println!("Solution: {:?}", soln);
}
