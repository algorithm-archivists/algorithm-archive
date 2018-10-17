package main

import (
	"fmt"
)

type point struct {
	x, y float64
}

func leftMostPoint(points []point) point {
	ret := points[0]

	for _, p := range points {
		if (p.x < ret.x) || (p.x == ret.x && p.y < ret.y) {
			ret = p
		}
	}

	return ret
}

func (p point) equal(o point) bool {
	return p.x == o.x && p.y == o.x
}

func counterClockWise(p1, p2, p3 point) bool {
	return (p3.y-p1.y)*(p2.x-p1.x) >= (p2.y-p1.y)*(p3.x-p1.x)
}

func jarvisMarch(points []point) []point {
	hullPoints := make([]point, 0)
	hullPoint := leftMostPoint(points)
	hullPoints = append(hullPoints, hullPoint)

	for {
		endPoint := points[0]

		for _, p := range points[1:] {
			if endPoint.equal(hullPoint) || !counterClockWise(p, hullPoints[len(hullPoints)-1], endPoint) {
				endPoint = p
			}
		}

		hullPoint = endPoint

		if endPoint.equal(hullPoints[0]) {
			break
		}

		hullPoints = append(hullPoints, hullPoint)
	}
	return hullPoints
}

func main() {
	points := []point{{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
		{-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9},
		{7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8},
	}

	hullPoints := jarvisMarch(points)
	fmt.Println("The hull points are:")

	for _, p := range hullPoints {
		fmt.Printf("x=%f y=%f\n", p.x, p.y)
	}
}
