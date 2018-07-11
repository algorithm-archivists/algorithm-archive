function verlet(pos::Float64, acc::Float64, dt::Float64)
    prev_pos = pos
    time = 0.0

    while (pos > 0)
        time += dt
        temp_pos = pos
        pos = pos * 2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos
    end

    println(time)
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
        vel += acc*dt
    end

    println(time)
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

    println(time)
end

function main()
    verlet(5.0, -10.0, 0.01);
    stormer_verlet(5.0, -10.0, 0.01);
    velocity_verlet(5.0, -10.0, 0.01);
end

main()
