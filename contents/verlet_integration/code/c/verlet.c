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
    printf("[#]\nTime for Verlet integration is:\n");
    printf("%lf\n", time);

    stormer_verlet(&time, &vel, 5.0, -10, 0.01);
    printf("[#]\nTime for Stormer Verlet integration is:\n");
    printf("%lf\n", time);
    printf("[#]\nVelocity for Stormer Verlet integration is:\n");
    printf("%lf\n", vel);

    velocity_verlet(&time, &vel, 5.0, -10, 0.01);
    printf("[#]\nTime for velocity Verlet integration is:\n");
    printf("%lf\n", time);
    printf("[#]\nVelocity for Stormer Verlet integration is:\n");
    printf("%lf\n", vel);

    return 0;
}
