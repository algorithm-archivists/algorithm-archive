#include <stdio.h>

void verlet(double pos, double acc, double dt) {
    double prev_pos, temp_pos, time;
    prev_pos = pos;
    time = 0;

    while (pos > 0) {
        time += dt;
        temp_pos = pos;
        pos = pos * 2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

   printf("%f\n", time);
}

void stormer_verlet(double pos, double acc, double dt) {
    double prev_pos, temp_pos, time, vel;
    prev_pos = pos;
    vel = 0;
    time = 0;
    while (pos > 0) {
        time += dt;
        temp_pos = pos;
        pos = pos * 2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        vel += acc * dt;
    }

   printf("%f\n", time);
}

void velocity_verlet(double pos, double acc, double dt) {
    double time, vel;
    vel = 0;
    time = 0;
    while (pos > 0) {
        time += dt;
        pos += vel * dt + 0.5 * acc * dt * dt;
        vel += acc * dt;
    }

   printf("%f\n", time);
}

int main() {
    verlet(5.0, -10, 0.01);
    stormer_verlet(5.0, -10, 0.01);
    velocity_verlet(5.0, -10, 0.01);

    return 0;
}

