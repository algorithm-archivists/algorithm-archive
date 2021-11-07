def verlet(pos, acc, dt)

    prev_pos = pos
    time = 0
    while pos > 0 do
        time += dt
        temp_pos = pos
        pos = pos*2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos
    end

   return time

end

def stormer_verlet(pos, acc, dt)

    prev_pos = pos
    vel = 0
    time = 0
    while pos > 0 do
        time += dt
        temp_pos = pos
        pos = pos*2 - prev_pos + acc * dt * dt
        prev_pos = temp_pos

        vel += acc*dt
    end

   return time, vel

end

def velocity_verlet(pos, acc, dt)

    vel = 0
    time = 0
    while pos > 0 do
        time += dt
        pos += vel*dt + 0.5*acc * dt * dt
        vel += acc*dt
    end

   return time, vel

end

puts "[#]\nTime for Verlet integration is:"
p verlet(5.0, -10, 0.01)

time, vel = stormer_verlet(5.0, -10, 0.01)
puts "[#]\nTime for Stormer Verlet integration is:"
p time
puts "[#]\nVelocity for Stormer Verlet integration is:"
p vel

time, vel = velocity_verlet(5.0, -10, 0.01)
puts "[#]\nTime for velocity Verlet integration is:"
p time
puts "[#]\nVelocity for velocity Verlet integration is:"
p vel
