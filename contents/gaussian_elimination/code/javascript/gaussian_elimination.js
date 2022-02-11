function gaussianElimination(a) {
  const rows = a.length
  const cols = a[0].length
  let row = 0;
  for (let col = 0; col < cols - 1; ++col) {

    let pivot = row;
    for (let i = row + 1; i < rows; ++i) {
      if (Math.abs(a[i][col]) > Math.abs(a[pivot][col])) {
        pivot = i;
      }
    }

    if (a[pivot][col] === 0) {
      console.log("The matrix is singular.");
      continue;
    }

    if (col !== pivot) {
      const t = a[col];
      a[col] = a[pivot];
      a[pivot] = t;
    }

    for (let i = row + 1; i < rows; ++i) {
      const scale = a[i][col] / a[row][col];

      for (let j = col + 1; j < cols; ++j) {
        a[i][j] -= a[row][j] * scale;
      }

      a[i][col] = 0;
    }

    ++row;
  }
  return a;
}

function backSubstitution(a) {
  const rows = a.length;
  const cols = a[0].length;
  const sol = [];

  for (let i = rows - 1; i >= 0; --i) {

    let sum = 0;
    for (let j = cols - 2; j > i; --j) {
      sum += sol[j] * a[i][j];
    }

    sol[i] = (a[i][cols - 1] - sum) / a[i][i];
  }
  return sol;
}

function gaussJordan(a) {
  const cols = a[0].length;
  let row = 0;

  for (let col = 0; col < cols - 1; ++col) {
    if (a[row][col] !== 0) {
      for (let i = cols - 1; i > col - 1; --i) {
        a[row][i] /= a[row][col];
      }

      for (let i = 0; i < row; ++i) {
        for (let j = cols - 1; j > col - 1; --j) {
          a[i][j] -= a[i][col] * a[row][j];
        }
      }

      ++row;
    }
  }
}

function printMatrixRow(row) {
  const text = row
    .map(v => (v < 0 ? " " : "  ") + v.toPrecision(8))
    .join("");

  console.log(text);
}

function printMatrix(a) {
  for (const row of a) {
    printMatrixRow(row);
  }
}

const a = [
  [3,  2, -4,  3],
  [2,  3,  3, 15],
  [5, -3,  1, 14]
];

gaussianElimination(a);
console.log("Gaussian elimination:");
printMatrix(a);

gaussJordan(a);
console.log("\nGauss-Jordan:");
printMatrix(a);

const sol = backSubstitution(a);
console.log("\nSolutions are:");
printMatrixRow(sol);
