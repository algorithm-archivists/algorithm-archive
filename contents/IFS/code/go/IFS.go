/*
This will generate a Sierpinski triangle with a chaos game of n points for an
initial triangle with three points on the vertices of an equilateral triangle:
    A = (0.0, 0.0)
    B = (0.5, sqrt(0.75))
    C = (1.0, 0.0)
It will output the file sierpinski.dat, which can be plotted after
*/
package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"time"
)

type point struct {
	x, y float64
}

func randPoint(points []point) point {
	return points[rand.Int()%len(points)]
}

func chaosGame(n int, shapePoints []point) []point {
	if n < 1 || len(shapePoints) == 0 {
		return nil
	}

	outputPoints := make([]point, n)

	curPoint := point{rand.Float64(), rand.Float64()}

	for i := range outputPoints {
		outputPoints[i] = curPoint
		rp := randPoint(shapePoints)
		curPoint.x = 0.5 * (curPoint.x + rp.x)
		curPoint.y = 0.5 * (curPoint.y + rp.y)
	}

	return outputPoints
}

func writePointsToFile(points []point, filePath string) error {
	file, err := os.Create(filePath)
	if err != nil {
		return fmt.Errorf("could not create file '%s': %s", filePath, err.Error())
	}
	defer file.Close()

	for _, p := range points {
		_, err = file.WriteString(fmt.Sprintf("%f\t%f\n", p.x, p.y))
		if err != nil {
			return fmt.Errorf("could not write to file '%s': %s", filePath, err.Error())
		}
	}

	return nil
}

func init() {
	rand.Seed(time.Now().UnixNano())
}

func main() {
	shapePoints := []point{
		{0.0, 0.0},
		{0.5, math.Sqrt(0.75)},
		{1.0, 0.0},
	}

	outputPoints := chaosGame(10000, shapePoints)

	filePath := "sierpinski.dat"
	err := writePointsToFile(outputPoints, filePath)
	if err != nil {
		fmt.Println("ERROR: " + err.Error())
		os.Exit(1)
	}
}
