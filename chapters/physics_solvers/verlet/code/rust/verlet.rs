fn verlet(mut pos: f64, acc: f64, dt: f64) {
    let mut prev_pos = pos;
    let mut time = 0.0;

    while pos > 0.0 {
        time += dt;
        let temp_pos = pos;
        pos = pos * 2.0 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

    println!("{}", time);
}

fn stormer_verlet(mut pos: f64, acc: f64, dt: f64) {
    let mut prev_pos = pos;
    let mut time = 0.0;

    while pos > 0.0 {
        time += dt;
        let temp_pos = pos;
        pos = pos * 2.0 - prev_pos + acc * dt * dt;
        prev_pos = temp_pos;
    }

    println!("{}", time);
}

fn velocity_verlet(mut pos: f64, acc: f64, dt: f64) {
    let mut time = 0.0;
    let mut vel = 0.0;

    while pos > 0.0 {
        time += dt;
        pos += vel * dt + 0.5 * acc * dt * dt;
        vel += acc * dt;
    }

    println!("{}", time);
}

fn main() {
    verlet(5.0, -10.0, 0.01);
    stormer_verlet(5.0, -10.0, 0.01);
    velocity_verlet(5.0, -10.0, 0.01);
}
