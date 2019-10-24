struct Point {
	x int
	y int
}

fn left_most_point(points []Point) Point {
	mut ret := points[0]

	for p in points {
		if (p.x < ret.x) || (p.x == ret.x && p.y < ret.y) {
			ret = p
		}
	}

	return ret
}

fn (p Point) equal(o Point) bool {
	return p.x == o.x && p.y == o.x
}

fn counter_clock_wise(p1, p2, p3 Point) bool {
	return (p3.y-p1.y) * (p2.x-p1.x) >= (p2.y-p1.y) * (p3.x-p1.x)
}

fn jarvis_march(points []Point) []Point {
	mut hull_point := left_most_point(points)
	mut hull_points := [hull_point]


	for {
		mut end_point := points[0]

		for i := 1; i < points.len; i++ {
			if end_point.equal(points[i]) || !counter_clock_wise(points[i], hull_points[hull_points.len-1], end_point) {
				end_point = points[i]
			}
		}

		hull_point = end_point
		if end_point.equal(hull_points[0]) {
			break
		}

		hull_points << hull_point
	}
	return hull_points
}

fn main() {
	points := [
		Point{-5, 2}, Point{5, 7}, Point{-6, -12}, Point{-14, -14}, Point{9, 9},
		Point{-1, -1}, Point{-10, 11}, Point{-6, 15}, Point{-6, -8}, Point{15, -9},
		Point{7, -7}, Point{-2, -9}, Point{6, -5}, Point{0, 14}, Point{2, 8}
	]

	hull_points := jarvis_march(points)

	println('The hull points are:')
	for p in hull_points {
		println('x=$p.x y=$p.y')
	}
}
