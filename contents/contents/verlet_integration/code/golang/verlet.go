package main

import "fmt"

func verlet(pos, acc, dt float64) (time float64) {
	prevPos := pos
	time = 0

	for pos > 0 {
		time += dt
		nextPos := pos*2 - prevPos + acc*dt*dt
		prevPos, pos = pos, nextPos
	}

	return
}

func stormerVerlet(pos, acc, dt float64) (time, vel float64) {
	prevPos := pos
	time, vel = 0, 0

	for pos > 0 {
		time += dt
		vel += acc * dt
		nextPos := pos*2 - prevPos + acc*dt*dt
		prevPos, pos = pos, nextPos
	}

	return
}

func velocityVerlet(pos, acc, dt float64) (time, vel float64) {
	time, vel = 0, 0

	for pos > 0 {
		time += dt
		pos += vel*dt + .5*acc*dt*dt
		vel += acc * dt
	}

	return
}

func main() {
	time := verlet(5., -10., .01)
	fmt.Println("Verlet")
	fmt.Println("Time:", time)
	fmt.Println()

	time, vel := stormerVerlet(5., -10., .01)
	fmt.Println("Stormer-Verlet")
	fmt.Println("Time:", time)
	fmt.Println("Velocity:", vel)
	fmt.Println()

	time, vel = velocityVerlet(5., -10., .01)
	fmt.Println("Velocity Verlet")
	fmt.Println("Time:", time)
	fmt.Println("Velocity:", vel)
}
