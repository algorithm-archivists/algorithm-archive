function verlet(pos, acc, dt){

    var prev_pos, temp_pos, time;
    prev_pos = pos;
    time = 0;

    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

   return time;

}

function stormer_verlet(pos, acc, dt){

    var prev_pos, temp_pos, time, vel;
    prev_pos = pos;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        temp_pos = pos;
        pos = pos*2 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        vel += acc*dt;
    }

   return time;

}

function velocity_verlet(pos, acc, dt){

    var time, vel;
    vel = 0;
    time = 0;
    while (pos > 0){
        time += dt;
        pos += vel*dt + 0.5*acc * dt * dt;
        vel += acc*dt;
    }

   return time;

}

console.log(verlet(5.0, -10, 0.01));
console.log(stormer_verlet(5.0, -10, 0.01));
console.log(velocity_verlet(5.0, -10, 0.01));

