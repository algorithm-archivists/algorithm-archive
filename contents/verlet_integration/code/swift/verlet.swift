func verlet(pos: Double, acc: Double, dt: Double) -> Double {
    var pos = pos
    var temp_pos, time: Double
    var prev_pos = pos
    time = 0.0
    
    while (pos > 0) {
        time += dt
        temp_pos = pos
        pos = pos*2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos
    }
    
    return time
}

func stormerVerlet(pos: Double, acc: Double, dt: Double) -> (time: Double, vel: Double) {
    var pos = pos
    var temp_pos, time, vel: Double
    var prev_pos = pos
    vel = 0
    time = 0
    
    while (pos > 0) {
        time += dt
        temp_pos = pos
        pos = pos*2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos
        
        vel += acc*dt
    }
    
    return (time:time, vel:vel)
}

func velocityVerlet(pos: Double, acc: Double, dt: Double) -> (time: Double, vel: Double) {
    var pos = pos
    var time, vel : Double
    vel = 0
    time = 0
    
    while (pos > 0) {
        time += dt
        pos += vel*dt + 0.5*acc * dt * dt
        vel += acc*dt
    }
    
    return (time:time, vel:vel)
}

func main() {
    let verletTime = verlet(pos: 5.0, acc: -10.0, dt: 0.01)
    print("Time for Verlet integration is: \(verletTime)")
    
    let stormer = stormerVerlet(pos: 5.0, acc: -10.0, dt: 0.01);
    print("Time for Stormer Verlet integration is: \(stormer.time)")
    print("Velocity for Stormer Verlet integration is: \(stormer.vel)")
    
    let velVerlet = velocityVerlet(pos: 5.0, acc: -10, dt: 0.01)
    print("Time for velocity Verlet integration is: \(velVerlet.time)")
    print("Velocity for velocity Verlet integration is: \(velVerlet.vel)")
}

main()
