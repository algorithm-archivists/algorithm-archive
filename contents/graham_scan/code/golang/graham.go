package main

import (
	"fmt"
	"math"
	"sort"
)

type point struct {
	x, y int
}

func counterClockwise(p1, p2, p3 point) bool {
	return (p3.y-p1.y)*(p2.x-p1.x) >= (p2.y-p1.y)*(p3.x-p1.x)
}

func polarAngle(ref, point point) float64 {
	return math.Atan2(float64(point.y-ref.y), float64(point.x-ref.x))
}

func grahamScan(points []point) []point {
	sort.Slice(points, func(a, b int) bool {
		return points[a].y < points[b].y || (points[a].y == points[b].y && points[a].x < points[b].x)
	})

	start := points[0]
	points = points[1:]

	sort.Slice(points, func(a, b int) bool {
		return polarAngle(start, points[a]) < polarAngle(start, points[b])
	})

	hull := []point{start, points[0], points[1]}
	for _, p := range points[2:] {
		for !counterClockwise(hull[len(hull)-2], hull[len(hull)-1], p) {
			hull = hull[:len(hull)-1]
		}
		hull = append(hull, p)
	}

	return hull
}

func main() {
	points := []point{{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
		{-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9},
		{7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8}}

	fmt.Println("The points in the hull are:")
	hull := grahamScan(points)
	for _, p := range hull {
		fmt.Println(p)
	}
}
