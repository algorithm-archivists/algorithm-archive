#include <iostream>

// Simple function for velocity-verlet
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

// Simple function for stormer-verlet
double stormer_verlet(double pos, double acc, double dt) {

  double prev_pos = pos;
  double time = 0;
  double vel = 0;
  while (pos > 0) {
    time += dt;
    double next_pos = pos * 2 - prev_pos + acc * dt * dt;
    prev_pos = pos;
    pos = next_pos;

    // The acceleration is constant, so the velocity is straightforward
    vel += acc * dt;
  }

  return time;
}

double velocity_verlet(double pos, double acc, double dt) {

  double time = 0;
  double vel = 0;
  while (pos > 0) {
    time += dt;
    pos += vel * dt + 0.5 * acc * dt * dt;
    vel += acc * dt;
  }

  return time;
}

int main() {

  // Note that depending on the simulation, you might want to have the verlet
  // loop outside.

  // For example, if your acceleration chages as a function of time, you might
  // need to also change the acceleration to be read into each of these
  // functions
  std::cout << verlet(5.0, -10, 0.01) << std::endl;
  std::cout << stormer_verlet(5.0, -10, 0.01) << std::endl;
  std::cout << velocity_verlet(5.0, -10, 0.01) << std::endl;
}
