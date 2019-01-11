fn verlet(mut pos: f64, acc: f64, dt: f64) -> f64 {
    let mut prev_pos = pos;
    let mut time = 0.0;

    while pos > 0.0 {
        time += dt;
        let temp_pos = pos;
        pos = pos * 2.0 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

    time
}

fn stormer_verlet(mut pos: f64, acc: f64, dt: f64) -> (f64, f64) {
    let mut prev_pos = pos;
    let mut time = 0.0;
    let mut vel = 0.0;

    while pos > 0.0 {
        time += dt;
        let temp_pos = pos;
        pos = pos * 2.0 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;

        // Because acceleration is constant, velocity is
        // straightforward
        vel += acc * dt;
    }

    (time, vel)
}

fn velocity_verlet(mut pos: f64, acc: f64, dt: f64) -> (f64, f64) {
    let mut time = 0.0;
    let mut vel = 0.0;

    while pos > 0.0 {
        time += dt;
        pos += vel * dt + 0.5 * acc * dt * dt;
        vel += acc * dt;
    }

    (time, vel)
}

fn main() {
    let time_v = verlet(5.0, -10.0, 0.01);
    let (time_sv, vel_sv) = stormer_verlet(5.0, -10.0, 0.01);
    let (time_vv, vel_vv) = velocity_verlet(5.0, -10.0, 0.01);

    println!("Time for original Verlet integration: {}", time_v);
    println!(
        "Time and velocity for Stormer Verlet integration: {}, {}",
        time_sv, vel_sv
    );
    println!(
        "Time and velocity for velocity Verlet integration: {}, {}",
        time_vv, vel_vv
    );
}
