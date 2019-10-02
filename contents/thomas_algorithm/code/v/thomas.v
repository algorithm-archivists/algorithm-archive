fn thomas(a []f32, b []f32, c mut []f32, d mut []f32) []f32 {
	c[0] = c[0] / b[0]
	d[0] = d[0] / b[0]

	for i := 1; i < d.len; i++ {
		scale := 1. / (b[i] - c[i-1]*a[i])
		c[i] *= scale
		d[i] = (d[i] - a[i]*d[i-1]) * scale
	}

	for i := d.len - 2; i >= 0; i-- {
		d[i] -= c[i] * d[i+1]
	}

	return d
}

fn main() {
	a := [0.0, 2.0, 3.0]
	b := [1.0, 3.0, 6.0]
	mut c := [4.0, 5.0, 0.0]
	mut d := [7.0, 5.0, 3.0]

	println("The system,")
	println("[1.0  4.0  0.0][x] = [7.0]")
	println("[2.0  3.0  5.0][y] = [5.0]")
	println("[0.0  3.0  6.0][z] = [3.0]")
	println("has the solution:")
	solve := thomas(a, b, mut c, mut d)
	for i in solve {
		println("[$i]")
	}
}