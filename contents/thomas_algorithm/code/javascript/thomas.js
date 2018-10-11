function thomas(a, b, c, d) {
	const size = a.length;

	c[0] /= b[0];
	d[0] /= b[0];

	for(let i = 1; i < size; ++i ) {
		let scale = 1 / (b[i] - c[i-1] * a[i]);
		c[i] *= scale;
		d[i] = (d[i] - a[i] * d[i-1]) * scale;
	}

	for(let i = size-2; i>=0; --i) {
		d[i] -= c[i] * d[i+1];
	}

	return d;
}

a = [0, 2, 3];
b = [1, 3, 6];
c = [4, 5, 0];
d = [7, 5, 3];

sol = thomas(a,b,c,d);
console.log(sol);
