const Complex = require("complex.js");

function dft(x) {
  const N = x.length;

  // Initialize an array with N elements, filled with 0s
  return Array(N)
    .fill(new Complex(0, 0))
    .map((temp, i) => {
      // Reduce x into the sum of x_k * exp(-2*sqrt(-1)*pi*i*k/N)
      return x.reduce((a, b, k) => {
        return a.add(b.mul(new Complex(0, (-2 * Math.PI * i * k) / N).exp()));
      }, new Complex(0, 0)); // Start accumulating from 0
    });
}

function cooley_tukey(x) {
  const N = x.length;
  const half = Math.floor(N / 2);
  if (N <= 1) {
    return x;
  }

  // Extract even and odd indexed elements with remainder mod 2
  const evens = cooley_tukey(x.filter((_, idx) => !(idx % 2)));
  const odds = cooley_tukey(x.filter((_, idx) => idx % 2));

  // Fill an array with null values
  let temp = Array(N).fill(null);

  for (let i = 0; i < half; i++) {
    const arg = odds[i].mul(new Complex(0, (-2 * Math.PI * i) / N).exp());

    temp[i] = evens[i].add(arg);
    temp[i + half] = evens[i].sub(arg);
  }

  return temp;
}

function bit_reverse_idxs(n) {
  if (!n) {
    return [0];
  } else {
    const twice = bit_reverse_idxs(n - 1).map(x => 2 * x);
    return twice.concat(twice.map(x => x + 1));
  }
}

function bit_reverse(x) {
  const N = x.length;
  const indexes = bit_reverse_idxs(Math.log2(N));
  return x.map((_, i) => x[indexes[i]]);
}

// Assumes log_2(N) is an integer
function iterative_cooley_tukey(x) {
  const N = x.length;

  x = bit_reverse(x);

  for (let i = 1; i <= Math.log2(N); i++) {
    const stride = 2 ** i;
    const half = stride / 2;
    const w = new Complex(0, (-2 * Math.PI) / stride).exp();
    for (let j = 0; j < N; j += stride) {
      let v = new Complex(1, 0);
      for (let k = 0; k < half; k++) {
        // perform butterfly multiplication
        x[k + j + half] = x[k + j].sub(v.mul(x[k + j + half]));
        x[k + j] = x[k + j].sub(x[k + j + half].sub(x[k + j]));
        // accumulate v as powers of w
        v = v.mul(w);
      }
    }
  }

  return x;
}

// Check if two arrays of complex numbers are approximately equal
function approx(x, y, tol = 1e-12) {
  let diff = 0;
  for (let i = 0; i < x.length; i++) {
    diff += x[i].sub(y[i]).abs();
  }
  return diff < tol;
}

const X = Array.from(Array(8), () => new Complex(Math.random(), 0));
const Y = cooley_tukey(X);
const Z = iterative_cooley_tukey(X);
const T = dft(X);

// Check if the calculations are correct within a small tolerance
console.log("Cooley tukey approximation is accurate: ", approx(Y, T));
console.log("Iterative cooley tukey approximation is accurate: ", approx(Z, T));
