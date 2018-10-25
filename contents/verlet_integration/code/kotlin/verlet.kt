data class VerletValues(val time: Double, val vel: Double)

fun verlet(_position: Double, acceleration: Double, dt: Double): Double {
    var position = _position  // Since function parameter are val and can't be modified
    var previousPosition = position
    var time = 0.0

    while (position > 0) {
        time += dt
        val nextPosition = position * 2 - previousPosition + acceleration * dt * dt
        previousPosition = position
        position = nextPosition
    }
    return time
}

fun stormerVerlet(_position: Double, acceleration: Double, dt: Double): VerletValues {
    var position = _position
    var previousPosition = position
    var time = 0.0
    var velocity = 0.0
    while (position > 0) {
        time += dt
        val nextPosition = position * 2 - previousPosition + acceleration * dt * dt
        previousPosition = position
        position = nextPosition
        velocity += acceleration * dt
    }
    return VerletValues(time, velocity)
}

fun velocityVerlet(_position: Double, acceleration: Double, dt: Double): VerletValues {
    var position = _position
    var time = 0.0
    var velocity = 0.0
    while (position > 0) {
        time += dt
        position += velocity * dt + 0.5 * acceleration * dt * dt
        velocity += acceleration * dt
    }
    return VerletValues(time, velocity)
}

fun main(args: Array<String>) {
    val verletTime = verlet(5.0, -10.0, 0.01)
    println("Time for Verlet integration is: $verletTime")

    val stormerVerlet = stormerVerlet(5.0, -10.0, 0.01)
    println("Time for Stormer Verlet integration is: " + stormerVerlet.time)
    println("Velocity for Stormer Verlet integration is: " + stormerVerlet.vel)

    val velocityVerlet = velocityVerlet(5.0, -10.0, 0.01)
    println("Time for velocity Verlet integration is: " + velocityVerlet.time)
    println("Velocity for velocity Verlet integration is: " + velocityVerlet.vel)
}
