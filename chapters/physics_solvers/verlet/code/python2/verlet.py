# KingFredrickVI
class Ball:

    def __init__(self, pos=0, vel=0, acc=0):
        self.pos = pos
        self.vel = vel
        self.acc = acc


class Simulation:

    def __init__(self, obj, dt = 0.01):
        self.obj = obj
        self.dt = dt
        self.prev_pos = self.obj.pos

    def run(self):
        self.time = 0

        while self.obj.pos > 0:
            self.time += self.dt
            self.step()

    def step(self):
        pass


class Verlet(Simulation):
    
    def step(self):
        temp_pos = self.obj.pos
        self.obj.pos = self.obj.pos * 2 - self.prev_pos + self.obj.acc * self.dt * self.dt
        self.prev_pos = temp_pos

class Stormer_Verlet(Simulation):

    def step(self):
        temp_pos = self.obj.pos
        self.obj.pos = self.obj.pos * 2 - self.prev_pos + self.obj.acc * self.dt * self.dt
        self.prev_pos = temp_pos

        self.obj.vel += self.obj.acc * self.dt

class Velocity_Verlet(Simulation):

    def step(self):
        self.obj.pos += self.obj.vel * self.dt + 0.5 * self.obj.acc * self.dt * self.dt
        self.obj.vel += self.obj.acc * self.dt


def main():
    sim = Verlet(Ball(pos = 5.0, acc = -10))
    sim.run()

    print "Verlet:", sim.time

    sim = Stormer_Verlet(Ball(pos = 5.0, acc = -10))
    sim.run()

    print "Stormer Verlet:", sim.time

    sim = Velocity_Verlet(Ball(pos = 5.0, acc = -10))
    sim.run()

    print "Velocity Verlet:", sim.time


if __name__ == "__main__":
    main()



