// Submitted by Gathros
#include <stdio.h>

// Simple function for velocity-verlet
void verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
    double prev_pos, temp_pos, time;
    prev_pos = pos;
    time = 0;

    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

   printf("%f\n", time);

}

// Simple function for stormer-verlet
void stormer_verlet(double pos, double acc, double dt){

    double prev_pos, temp_pos, time, vel;
    prev_pos = pos;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        // The acceleration is constant, so the velocity is straightforward
        vel += acc*dt;
    }

   printf("%f\n", time);

}

// Simple function for velocity-verlet
void velocity_verlet(double pos, double acc, double dt){

    double time, vel;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        pos += vel*dt + 0.5*acc * dt * dt;
        vel += acc*dt;
    }

   printf("%f\n", time);

}

int main(){
    verlet(5.0, -10, 0.01);
    stormer_verlet(5.0, -10, 0.01);
    velocity_verlet(5.0, -10, 0.01);

}

