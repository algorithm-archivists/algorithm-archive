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
    print("Verlet")
    print("Time: {:.10f}".format(time))
    print()

    time, vel = stormer_verlet(5, -10, 0.01)
    print("Stormer-Verlet")
    print("Time: {:.10f}".format(time))
    print("Velocity: {:.10f}".format(vel))
    print()

    time, vel = velocity_verlet(5, -10, 0.01)
    print("Velocity Verlet")
    print("Time: {:.10f}".format(time))
    print("Velocity: {:.10f}".format(vel))
    print()

if __name__ == '__main__':
    main()
