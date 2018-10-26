function verlet(pos, acc, dt) {
  let prevPos = pos;
  let time = 0;
  let tempPos;

  while (pos > 0) {
    time += dt;
    tempPos = pos;
    pos = pos * 2 - prevPos + acc * dt * dt;
    prevPos = tempPos;
  }

  return time;
}

function stormerVerlet(pos, acc, dt) {
  let prevPos = pos;
  let time = 0;
  let vel = 0;
  let tempPos;

  while (pos > 0) {
    time += dt;
    tempPos = pos;
    pos = pos * 2 - prevPos + acc * dt * dt;
    prevPos = tempPos;

    vel += acc * dt;
  }

  return { time, vel };
}

function velocityVerlet(pos, acc, dt) {
  let time = 0;
  let vel = 0;

  while (pos > 0) {
    time += dt;
    pos += vel * dt + 0.5 * acc * dt * dt;
    vel += acc * dt;
  }

  return { time, vel };
}

const time = verlet(5, -10, 0.01);
console.log(`Time for Verlet integration is: ${time}\n`);

const stormer = stormerVerlet(5, -10, 0.01);
console.log(`Time for Stormer Verlet integration is: ${stormer.time}`);
console.log(`Velocity for Stormer Verlet integration is: ${stormer.vel}\n`);

const velocity = velocityVerlet(5, -10, 0.01);
console.log(`Time for Velocity Verlet integration is: ${velocity.time}`);
console.log(`Velocity for Velocity Verlet integration is: ${velocity.vel}\n`);
