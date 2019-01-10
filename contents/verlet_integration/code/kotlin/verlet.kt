data class VerletValues(val time: Double, val vel: Double)

fun verlet(_pos: Double, acc: Double, dt: Double): Double {
    var pos = _pos  // Since function parameter are val and can't be modified
    var prevPos = pos
    var time = 0.0

    while (pos > 0) {
        time += dt
        val nextPos = pos * 2 - prevPos + acc * dt * dt
        prevPos = pos
        pos = nextPos
    }
    return time
}

fun stormerVerlet(_pos: Double, acc: Double, dt: Double): VerletValues {
    var pos = _pos
    var prevPos = pos
    var time = 0.0
    var vel = 0.0
    while (pos > 0) {
        time += dt
        val nextPos = pos * 2 - prevPos + acc * dt * dt
        prevPos = pos
        pos = nextPos
        vel += acc * dt
    }
    return VerletValues(time, vel)
}

fun velocityVerlet(_pos: Double, acc: Double, dt: Double): VerletValues {
    var pos = _pos
    var time = 0.0
    var vel = 0.0
    while (pos > 0) {
        time += dt
        pos += vel * dt + 0.5 * acc * dt * dt
        vel += acc * dt
    }
    return VerletValues(time, vel)
}

fun main(args: Array<String>) {
    val verletTime = verlet(5.0, -10.0, 0.01)
    println("Time for Verlet integration is: $verletTime")

    val stormerVerlet = stormerVerlet(5.0, -10.0, 0.01)
    println("Time for Stormer Verlet integration is: $stormerVerlet.time")
    println("Velocity for Stormer Verlet integration is: $stormerVerlet.vel")

    val velocityVerlet = velocityVerlet(5.0, -10.0, 0.01)
    println("Time for Velocity Verlet integration is: $velocityVerlet.time")
    println("Velocity for Velocity Verlet integration is: $velocityVerlet.vel")
}
