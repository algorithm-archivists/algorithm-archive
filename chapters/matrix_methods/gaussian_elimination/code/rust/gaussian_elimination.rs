use std::fmt;

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

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..self.rows {
            if row != 0 { writeln!(f, "")?; }
            for col in 0..self.cols {
                if col != 0 { write!(f, " ")?; }
                write!(f, "{}", self.get(row, col))?;
            }
        }
        Ok(())
    }
}

fn main() {
    let mut mat = Matrix::new(3,3);

    for i in 0..3 {
        for j in 0..3 {
            mat.set(i, j, (i*j) as f64);
        }
    }

    mat.swap_rows(0, 1);

    println!("{}", mat);
}
