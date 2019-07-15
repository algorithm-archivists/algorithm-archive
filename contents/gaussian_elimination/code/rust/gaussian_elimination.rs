// submitted by jess 3jane

use std::cmp::min;
use std::ops::{Index, IndexMut};

pub struct Matrix {
    rows: usize,
    cols: usize,
    data: Vec<f64>,
}

impl Matrix {
    fn new(rows: usize, cols: usize, data: &[f64]) -> Matrix {
        Matrix {
            rows,
            cols,
            data: data.to_vec(),
        }
    }

    fn swap_rows(&mut self, a: usize, b: usize) {
        for col in 0..self.cols {
            self.data.swap(a * self.cols + col, b * self.cols + col);
        }
    }
}

impl Index<(usize, usize)> for Matrix {
    type Output = f64;
    fn index(&self, (row, col): (usize, usize)) -> &f64 {
        &self.data[row * self.cols + col]
    }
}

impl IndexMut<(usize, usize)> for Matrix {
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut f64 {
        &mut self.data[row * self.cols + col]
    }
}

fn gaussian_elimination(a: &mut Matrix) {
    for k in 0..min(a.cols, a.rows) {
        // find the maximum element for this column
        let mut max_row = k;
        let mut max_value = a[(k, k)].abs();
        for row in (k + 1)..a.rows {
            if max_value < a[(row, k)].abs() {
                max_value = a[(row, k)].abs();
                max_row = row;
            }
        }

        // Check to make sure the matrix is good
        if a[(max_row, k)] == 0.0 {
            println!("Matrix is singular, aborting");
            return;
        }

        // swap the row with the highest value for this kumn to the top
        a.swap_rows(k, max_row);

        // Loop over all remaining rows
        for i in k + 1..a.rows {
            // find the fraction
            let fraction = a[(i, k)] / a[(k, k)];

            // Loop through all columns for that row
            for j in (k + 1)..a.cols {
                // re-evaluate each element
                a[(i, j)] -= a[(k, j)] * fraction;
            }

            // set lower elements to 0
            a[(i, k)] = 0.0;
        }
    }
}

fn back_substitution(a: &Matrix) -> Vec<f64> {
    let mut soln = vec![0.0; a.rows];

    soln[a.rows - 1] = a[(a.rows - 1, a.cols - 1)] / a[(a.rows - 1, a.cols - 2)];

    for i in (0..a.rows - 1).rev() {
        let mut sum = 0.0;
        for j in (i..a.rows).rev() {
            sum += soln[j] * a[(i, j)];
        }
        soln[i] = (a[(i, a.cols - 1)] - sum) / a[(i, i)];
    }

    soln
}

fn main() {
    // The example matrix from the text
    let mut a = Matrix::new(
        3,
        4,
        &vec![2.0, 3.0, 4.0, 6.0, 1.0, 2.0, 3.0, 4.0, 3.0, -4.0, 0.0, 10.0],
    );

    gaussian_elimination(&mut a);
    let soln = back_substitution(&a);
    println!("Solution: {:?}", soln);
}
