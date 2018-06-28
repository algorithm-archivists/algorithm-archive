# submitted by HugoGranstrom

def verlet_step(pos_prev, pos, acc, dt):
    pos_next = 2 * pos - pos_prev + acc * dt * dt
    return pos_next

# calculate velocity at the current timestep
def stormer_verlet_current(pos_prev, pos_next, dt):
    vel_current = (pos_next - pos_prev) / (2 * dt)
    return vel_current

#calculate velocity at the next timestep
def stormer_verlet_next(pos_next, pos, dt):
    vel_next = (pos_next - pos) / (dt)
    return vel_next

def acc_func(pos):
    # write the function for acceleration here, for example acc = F/m 
    # now acc is constant and independent of pos, thus pos is not used
    # for other implentations it can be used though
    return -10

def velocity_verlet_step(pos, vel, acc, dt):
    pos_next = pos + vel * dt + 0.5 * acc * dt * dt
    acc_next = acc_func(pos_next)
    vel_next = vel + 0.5 * (acc + acc_next) * dt
    return pos_next, vel_next, acc_next

# HugoGranstrom
# example calculating time it takes for an object to fall 5 m with acc = -10
# because acc is constant, all 0.5 * (acc + acc_next) * dt becomes just acc * dt

def run_verlet(pos_start, acc, dt):
    time = 0
    pos = pos_start
    pos_prev = pos_start

    while pos > 0:
        # calculate next timestep
        pos_next = 2 * pos - pos_prev + acc * dt * dt

        # prepare for next timestep
        pos_prev = pos
        pos = pos_next
        time += dt
    
    print(f"Classic Verlet took {time} seconds")

def run_velocity_verlet(pos_start, vel_start, acc, dt):
    time = 0
    pos = pos_start
    vel = vel_start

    while pos > 0:
        # calculate next timestep
        pos_next = pos + vel * dt + 0.5 * acc * dt * dt
        vel_next = vel + acc * dt

        # prepare for next timestep
        pos = pos_next
        vel = vel_next
        time += dt
    
    print(f"Velocity Verlet took {time} seconds")

run_verlet(5, -10, 0.01)
run_velocity_verlet(5, 0, -10, 0.01)