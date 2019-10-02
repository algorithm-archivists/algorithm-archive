import math

fn forward_euler(timestep f64, n int) []f64 {
    mut res := [f64(0.0)].repeat(n)
    res[0] = f64(1)
    for x := 1; x < n; x++ {
		res[x] = res[x-1] - 3.0*res[x-1]*timestep
	}
	return res
}

fn check(result []f64, threshold, timestep f64) bool {
	mut approx := true
	for x := 0.0; int(x) < result.len; x++ {
		solution := math.pow(math.E, -3.0 * x * timestep)
		if math.abs(result[int(x)]-solution) > threshold {
            tmp := result[int(x)]
			println(tmp)
			println(solution)
			approx = false
		}
	}
	return approx
}

fn main() {
	timestep := .01
    threshold := .01
	n := 100

	result := forward_euler(timestep, n)

	if check(result, threshold, timestep) {
		println("All values within threshold")
	} else {
		println("Value(s) not within threshold")
	}
}