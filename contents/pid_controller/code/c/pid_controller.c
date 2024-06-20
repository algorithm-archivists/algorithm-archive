#include <stdio.h>

struct pid_context {
    double kp;
    double ki;
    double kd;
    double setpoint;
    double last_error;
    double integral;
    double dt; // Normally you calculate the change in time.
};

struct pid_context get_pid(double setpoint, double dt, double kp, double ki,
                           double kd) {

    struct pid_context ctx = {0};
    ctx.setpoint = setpoint;
    ctx.dt = dt;
    ctx.kp = kp;
    ctx.ki = ki;
    ctx.kd = kd;

    return ctx;
}

double pid_calculate(struct pid_context ctx, double input) {
    // Here you would calculate the time elapsed.
    double error = ctx.setpoint - input;
    ctx.integral += error * ctx.dt;
    double derivative = (error - ctx.last_error) / ctx.dt;
    ctx.last_error = error;

    return ctx.kp * error + ctx.ki * ctx.integral + ctx.kd * derivative;
}

int main() {
    struct pid_context ctx = get_pid(1.0, 0.01, 1.2, 1.0, 0.001);
    double input = 0.0;

    for (int i = 0; i < 100; ++i) {
        input += pid_calculate(ctx, input);
        printf("%g\n", input);
    }

    return 0;
}
