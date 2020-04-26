// Package demonstrates Gaussian Elimination
package main

import (
	"fmt"
	"math"
)

func gaussianElimination(a [][]float64) {
	singular := false
	rows := len(a)
	cols := len(a[0])

	for c, r := 0, 0; c < cols && r < rows; c++ {
		// 1. find highest value in column below row to be pivot
		p, highest := r, 0.
		for i, row := range a[r:] {
			if abs := math.Abs(row[c]); abs > highest {
				p = r + i
				highest = abs
			}
		}
		highest = a[p][c] // correct sign

		if highest == 0. {
			if !singular {
				singular = true
				fmt.Println("This matrix is singular.")
			}
			continue
		}

		// 2. swap pivot with current row
		if p != r {
			a[r], a[p] = a[p], a[r]
		}

		for _, row := range a[r+1:] {
			// 3. find fraction from pivot value
			frac := row[c] / highest

			// 4. subtract row to set rest of column to zero
			for j := range row {
				row[j] -= frac * a[r][j]
			}

			// 5. ensure col goes to zero (no float rounding)
			row[c] = 0.
		}

		r++
	}
}

func gaussJordan(a [][]float64) {
	for r := len(a) - 1; r >= 0; r-- {
		// Find pivot col
		p := -1
		for c, cell := range a[r] {
			if cell != 0. {
				p = c
				break
			}
		}
		if p < 0 {
			continue
		}

		// Scale pivot r to 1.
		scale := a[r][p]
		for c := range a[r][p:] {
			a[r][p+c] /= scale
		}
		// Subtract pivot row from each row above
		for _, row := range a[:r] {
			scale = row[p]
			for c, cell := range a[r][p:] {
				row[p+c] -= cell * scale
			}
		}
	}
}

func backSubstitution(a [][]float64) []float64 {
	rows := len(a)
	cols := len(a[0])
	x := make([]float64, rows)
	for r := rows - 1; r >= 0; r-- {
		sum := 0.

		for c := cols - 2; c > r; c-- {
			sum += x[c] * a[r][c]
		}

		x[r] = (a[r][cols-1] - sum) / a[r][r]
	}
	return x
}

func printMatrixRow(row []float64) {
	fmt.Print("[")
	for _, cell := range row {
		fmt.Printf("%9.4f ", cell)
	}
	fmt.Println("]")
}

func printMatrix(a [][]float64) {
	for _, row := range a {
		printMatrixRow(row)
	}
	fmt.Println()
}

func main() {
	a := [][]float64{
		{2, 3, 4, 6},
		{1, 2, 3, 4},
		{3, -4, 0, 10},
	}
	fmt.Println("Original Matrix:")
	printMatrix(a)

	fmt.Println("Gaussian elimination:")
	gaussianElimination(a)
	printMatrix(a)

	gaussJordan(a)
	fmt.Println("Gauss-Jordan:")
	printMatrix(a)

	fmt.Println("Solutions are:")
	x := backSubstitution(a)
	printMatrixRow(x)
}
