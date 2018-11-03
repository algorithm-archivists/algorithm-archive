const complex = require('./complex');

function dft(X) {
  const len = X.length;
  const fft = []
  for (let i = 0; i < len; ++i) {
    fft[i] = complex.Complex(0,0);
    for (let j = 0; j < len; ++j) {
      fft[i]=fft[i].add(complex.Complex(0, -2.0 * Math.PI * i * j / len).exp().mul(X[j]));
    }
  }
  return fft;
}

function cooleyTukey(X) {
  const len = X.length;
  if (len <= 1) {
    return X;
  }
  const hlen = Math.floor(len/2); //so we dont have to redo this many times

  const Xeven = [];
  const Xodd = [];
  for (let i = 0; i < hlen; ++i) {
    Xodd[i] = X[2 * i + 1];
    Xeven[i] = X[2 * i];
  }
  const even = cooleyTukey(Xeven);
  const odd = cooleyTukey(Xodd);
  const fft = [];
  for (let i = 0; i < hlen; ++i) {
    fft[i] = complex.Complex(0, -2 * Math.PI * i / len).exp().mul(odd[i]).add(even[i]);
    fft[i+hlen] = complex.Complex(0, -2 * Math.PI * i / len).exp().mul(odd[i]).mul(-1).add(even[i]);
  }
  return fft;
}

function bitReverse(X) {
  const len = X.length;
  const lglen = Math.floor(Math.log2(len)) - 1;
  for (let i = 0; i < len; ++i) {
    let n = i;
    let a = i;
    let count = lglen;
    for (n >>= 1; n > 0; n >>= 1) {
      a = (a << 1) | (n & 1);
      --count;
    }
    n = (a << count) & ((2 << lglen) - 1);
    if (n > i) {
      const tmp = X[i];
      X[i] = X[n];
      X[n] = tmp;
    }
  }
  return X;
}

function iterativeCooleyTukey(X) {
  X = bitReverse(X);
  const len = X.length;

  for (let i = 1; i <= Math.floor(Math.log2(len)); ++i) {
    const stride = 1 << i;
    const hstride = 1 << (i - 1);
    const w = complex.Complex(0, -2 * Math.PI / stride).exp();
    for (let j = 0; j < len; j += stride) {
      let v = complex.Complex(1,0);
      for (let k = 0; k < hstride; ++k) {
        X[k + j + hstride] = X[k + j].sub(X[k + j + hstride].mul(v));
        X[k + j] = X[k + j].mul(2).sub(X[k + j + hstride]);
        v = v.mul(w);
      }
    }
  }
  return X;
}

let m=[];
for(let i=0;i<64;++i) {
  m[i]=complex.Complex(1/(i+1),0);
}
let C = cooleyTukey(m);
let D = dft(m);
let I = iterativeCooleyTukey(m);
for(let i=0;i<64;++i) {
  console.log(C[i]);
  console.log(D[i]);
  console.log(I[i]);
}
