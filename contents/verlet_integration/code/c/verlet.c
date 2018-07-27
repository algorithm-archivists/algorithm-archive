#include <stdio.h>

void verlet(double *time, double pos, double acc, double dt) {
    double prev_pos, temp_pos;
    prev_pos = pos;
    *time = 0.0;

    while (pos > 0) {
        *time += dt;
        temp_pos = pos;
        pos = pos * 2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }
}

void stormer_verlet(double *time, double *vel,
                    double pos, double acc, double dt) {
    double prev_pos, temp_pos;
    prev_pos = pos;
    *vel = 0.0;
    *time = 0.0;

    while (pos > 0) {
        *time += dt;
        temp_pos = pos;
        pos = pos * 2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        *vel += acc * dt;
    }
}

void velocity_verlet(double *time, double *vel,
                     double pos, double acc, double dt) {
    *vel = 0.0;
    *time = 0.0;

    while (pos > 0) {
        *time += dt;
        pos += (*vel) * dt + 0.5 * acc * dt * dt;
        *vel += acc * dt;
    }
}

int main() {
    double time, vel;

    verlet(&time, 5.0, -10, 0.01);
    printf("Time for Verlet integration is: %lf\n",
           time);

    stormer_verlet(&time, &vel, 5.0, -10, 0.01);
    printf("Time and velocity for Stormer Verlet integration is: %lf, %lf\n",
           time, vel);

    velocity_verlet(&time, &vel, 5.0, -10, 0.01);
    printf("Time and velocity for velocity Verlet integration is: %lf, %lf\n",
           time, vel);

    return 0;
}
