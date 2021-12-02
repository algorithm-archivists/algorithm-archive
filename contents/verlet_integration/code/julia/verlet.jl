function verlet(pos::Float64, acc::Float64, dt::Float64)
    prev_pos = pos
    time = 0.0

    while (pos > 0)
        time += dt
        temp_pos = pos
        pos = pos * 2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos
    end

    return time
end

function stormer_verlet(pos::Float64, acc::Float64, dt::Float64)
    prev_pos = pos
    time = 0.0
    vel = 0.0

    while (pos > 0.0)
        time += dt
        temp_pos = pos
        pos = pos * 2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos

        # Because acceleration is constant, velocity is straightforward
        vel += acc * dt
    end

    return time, vel
end

function velocity_verlet(pos::Float64, acc::Float64, dt::Float64)
    prev_pos = pos
    time = 0.0
    vel = 0.0

    while (pos > 0.0)
        time += dt
        pos += vel * dt + 0.5 * acc * dt * dt;
        vel += acc * dt;
    end

    return time, vel
end

function main()
    time = verlet(5.0, -10.0, 0.01);
    println("[#]\nTime for Verlet integration is:")
    println("$(time)")

    time, vel = stormer_verlet(5.0, -10.0, 0.01);
    println("[#]\nTime for Stormer Verlet integration is:")
    println("$(time)")
    println("[#]\nVelocity for Stormer Verlet integration is:")
    println("$(vel)")
    
    time, vel = velocity_verlet(5.0, -10.0, 0.01);
    println("[#]\nTime for velocity Verlet integration is:")
    println("$(time)")
    println("[#]\nVelocity for velocity Verlet integration is:")
    println("$(vel)")

end

main()
