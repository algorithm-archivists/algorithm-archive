fn thomas(a []f32, b []f32, c []f32, d []f32) []f32 {
	mut new_c := c
	mut new_d := d
	new_c[0] = new_c[0] / b[0]
	new_d[0] = new_d[0] / b[0]

	for i := 1; i < d.len; i++ {
		scale := 1. / (b[i] - new_c[i-1]*a[i])
		new_c[i] *= scale
		new_d[i] = (new_d[i] - a[i]*new_d[i-1]) * scale
	}

	for i := d.len - 2; i >= 0; i-- {
		new_d[i] -= new_c[i] * new_d[i+1]
	}

	return new_d
}

fn main() {
	a := [0.0, 2.0, 3.0]
	b := [1.0, 3.0, 6.0]
	c := [4.0, 5.0, 0.0]
	d := [7.0, 5.0, 3.0]

	println("The system,")
	println("[1.0  4.0  0.0][x] = [7.0]")
	println("[2.0  3.0  5.0][y] = [5.0]")
	println("[0.0  3.0  6.0][z] = [3.0]")
	println("has the solution:")
	solution := thomas(a, b, c, d)
	for i in solution {
		println("[$i]")
	}
}