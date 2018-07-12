// Submitted by lolatomroflsinnlos
static void verlet(double pos, double acc, double dt){

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

    System.out.println(time);

}

// Simple function for stormer-verlet
static void stormer_verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
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

    System.out.println(time);

}

// Simple function for velocity-verlet
static void velocity_verlet(double pos, double acc, double dt){

    // Note that we are using a temp variable for the previous position
    double time, vel;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        pos += vel*dt + 0.5*acc * dt * dt;
        vel += acc*dt;
    }

    System.out.println(time);

}

public static void main(String[] args) {

    verlet(5.0, -10, 0.01);
    stormer_verlet(5.0, -10, 0.01);
    velocity_verlet(5.0, -10, 0.01);

}

