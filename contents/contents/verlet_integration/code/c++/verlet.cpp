#include <iomanip>
#include <iostream>

struct timestep {
  double time;
  double vel;
};

double verlet(double pos, double acc, double dt) {

  double prev_pos = pos;
  double time = 0;

  while (pos > 0) {
    time += dt;
    double next_pos = pos * 2 - prev_pos + acc * dt * dt;
    prev_pos = pos;
    pos = next_pos;
  }

  return time;
}

timestep stormer_verlet(double pos, double acc, double dt) {

  double prev_pos = pos;
  double time = 0;
  double vel = 0;
  while (pos > 0) {
    time += dt;
    double next_pos = pos * 2 - prev_pos + acc * dt * dt;
    prev_pos = pos;
    pos = next_pos;

    // The acceleration is constant, so the velocity is
    // straightforward
    vel += acc * dt;
  }

  return timestep { time, vel };
}

timestep velocity_verlet(double pos, double acc, double dt) {

  double time = 0;
  double vel = 0;
  while (pos > 0) {
    time += dt;
    pos += vel * dt + 0.5 * acc * dt * dt;
    vel += acc * dt;
  }

  return timestep { time, vel };
}

int main() {
  std::cout << std::fixed << std::setprecision(8);

  // Note that depending on the simulation, you might want to have the
  // Verlet loop outside.

  // For example, if your acceleration chages as a function of time,
  // you might need to also change the acceleration to be read into
  // each of these functions.

  double time = verlet(5.0, -10, 0.01);
  std::cout << "Time for Verlet integration is: " \
            << time << std::endl;

  timestep timestep_sv = stormer_verlet(5.0, -10, 0.01);
  std::cout << "Time for Stormer Verlet integration is: " \
            << timestep_sv.time << std::endl;
  std::cout << "Velocity for Stormer Verlet integration is: " \
            << timestep_sv.vel << std::endl;

  timestep timestep_vv = velocity_verlet(5.0, -10, 0.01);
  std::cout << "Time for velocity Verlet integration is: " \
            << timestep_vv.time << std::endl;
  std::cout << "Velocity for velocity Verlet integration is: " \
            << timestep_vv.vel << std::endl;

  return 0;

}
