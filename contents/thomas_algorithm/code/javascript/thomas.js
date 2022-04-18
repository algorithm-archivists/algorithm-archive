function thomas(a, b, c, x) {
	const y = [];

	y[0] = c[0] / b[0];
	x[0] = x[0] / b[0];

	for (let i = 1; i < a.length; i++) {
		const scale = 1.0 / (b[i] - a[i] * y[i - 1]);
		y[i] = c[i] * scale;
		x[i] = (x[i] - a[i] * x[i - 1]) * scale;
	}

	for (let i = a.length - 2; i >= 0; i--)
		x[i] -= y[i] * x[i + 1];
}

let a = [0.0, 2.0, 3.0];
let b = [1.0, 3.0, 6.0];
let c = [4.0, 5.0, 0.0];
let x = [7.0, 5.0, 3.0];

console.log("The system,");
console.log("[1.0  4.0  0.0][x] = [7.0]");
console.log("[2.0  3.0  5.0][y] = [5.0]");
console.log("[0.0  3.0  6.0][z] = [3.0]");
console.log("has the solution:\n");

thomas(a, b, c, x);

for (let i = 0; i < 3; i++)
	console.log("[" + x[i] + "]");
