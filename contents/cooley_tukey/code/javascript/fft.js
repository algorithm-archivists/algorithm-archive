// This code example makes use of complex.js.
// You can install it with `npm install complex.js`.

const complex = require('complex.js');

function dft(x) {
    const len = x.length;
    const X = []

    for (let i = 0; i < len; ++i) {
	X[i] = complex.Complex(0,0);

	for (let j = 0; j < len; ++j) {
		X[i] = X[i].add(complex.Complex(0, -2.0 * Math.PI * i * j / len).exp().mul(x[j]));
	}
    }

    return X;
}

function cooleyTukey(x) {
    const len = x.length;
    if (len <= 1) {
	return x;
    }
    const hlen = Math.floor(len/2); //so we dont have to redo this many times
    
    let even = [];
    let odd = [];
    for (let i = 0; i < hlen; ++i) {
	odd[i] = x[2 * i + 1];
	even[i] = x[2 * i];
    }

    even = cooleyTukey(even);
    odd = cooleyTukey(odd);

    const X = [];
    for (let i = 0; i < hlen; ++i) {
	X[i] = complex.Complex(0, -2 * Math.PI * i / len).exp().mul(odd[i]).add(even[i]);
	X[i+hlen] = complex.Complex(0, -2 * Math.PI * i / len).exp().mul(odd[i]).mul(-1).add(even[i]);
    }

    return X;
}

function bitReverse(x) {
    const len = x.length;
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
	    const tmp = x[i];
	    x[i] = x[n];
	    x[n] = tmp;
	}
    }

    return x;
}

function iterativeCooleyTukey(x) {
    x = bitReverse(x);
    const len = x.length;

    for (let i = 1; i <= Math.floor(Math.log2(len)); ++i) {
	const stride = 1 << i;
	const hstride = 1 << (i - 1);
	const w = complex.Complex(0, -2 * Math.PI / stride).exp();

	for (let j = 0; j < len; j += stride) {
	    let v = complex.Complex(1,0);

	    for (let k = 0; k < hstride; ++k) {
		x[k + j + hstride] = x[k + j].sub(x[k + j + hstride].mul(v));
		x[k + j] = x[k + j].mul(2).sub(x[k + j + hstride]);
		v = v.mul(w);
	    }
	}
    }

    return x;
}

const x = [];
for (let i = 0; i < 64; ++i) {
    x[i] = complex.Complex(Math.random(),0);
}

const C = cooleyTukey(x);
const D = dft(x);
const I = iterativeCooleyTukey(x);

const bound = 10**-12;
for (let i = 0; i < 64; ++i) {
    if (D[i].sub(C[i]).abs() < bound && D[i].sub(I[i]).abs() < bound){
	console.log("X[" + i + "] = " + C[i].toString());
    } else {
	console.log("FFT first differs at index " + i);
	console.log("Cooley Tukey : " + C[i].toString());
	console.log("Normal dft : " + D[i].toString());
	console.log("Iterative Cooley Tukey : " + I[i].toString());
	break;
    }
}
