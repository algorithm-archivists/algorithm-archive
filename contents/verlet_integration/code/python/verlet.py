def verlet(pos, acc, dt):
    prev_pos = pos
    time = 0

    while pos > 0:
        time += dt
        next_pos = pos * 2 - prev_pos + acc * dt * dt
        prev_pos, pos = pos, next_pos

    return time

def stormer_verlet(pos, acc, dt):
    prev_pos = pos
    time = 0
    vel = 0

    while pos > 0:
        time += dt
        next_pos = pos * 2 - prev_pos + acc * dt * dt
        prev_pos, pos = pos, next_pos
        vel += acc * dt

    return time, vel

def velocity_verlet(pos, acc, dt):
    time = 0
    vel = 0

    while pos > 0:
        time += dt
        pos += vel * dt + 0.5 * acc * dt * dt
        vel += acc * dt

    return time, vel

def main():
    time = verlet(5, -10, 0.01)
    print("[#]\nTime for Verlet integration is:")
    print("{:.10f}".format(time))

    time, vel = stormer_verlet(5, -10, 0.01)
    print("[#]\nTime for Stormer Verlet integration is:")
    print("{:.10f}".format(time))
    print("[#]\nVelocity for Stormer Verlet integration is:")
    print("{:.10f}".format(vel))

    time, vel = velocity_verlet(5, -10, 0.01)
    print("[#]\nTime for velocity Verlet integration is:")
    print("{:.10f}".format(time))
    print("[#]\nVelocity for velocity Verlet integration is:")
    print("{:.10f}".format(vel))


if __name__ == '__main__':
    main()
