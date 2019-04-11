proc verlet(pos_in, acc, dt: float): float =
  var
    pos: float = pos_in
    prevPos: float = pos
    time: float = 0.0
    tempPos: float

  while pos > 0.0:
    time += dt
    tempPos = pos
    pos = pos * 2 - prevPos + acc * dt * dt
    prevPos = tempPos

  return time

proc stormerVerlet(pos_in, acc, dt: float): (float, float) =
  var
    pos: float = pos_in
    prevPos: float = pos
    time: float = 0.0
    vel: float = 0.0
    tempPos: float

  while pos > 0.0:
    time += dt
    tempPos = pos
    pos = pos * 2 - prevPos + acc * dt * dt
    prevPos = tempPos

    vel += acc * dt

  return (time, vel)

proc velocityVerlet(pos_in, acc, dt: float): (float, float) =
  var
    pos: float = pos_in
    time: float = 0.0
    vel: float = 0.0

  while pos > 0.0:
    time += dt
    pos += vel * dt + 0.5 * acc * dt * dt
    vel += acc * dt

  return (time, vel)

let timeV = verlet(5.0, -10.0, 0.01)
echo "Time for Verlet integration is: ", timeV

let (timeSV, velSV) = stormerVerlet(5.0, -10.0, 0.01)
echo "Time for Stormer Verlet integration is: ", timeSV
echo "Velocity for Stormer Verlet integration is: ", velSV

let (timeVV, velVV) = velocityVerlet(5.0, -10.0, 0.01)
echo "Time for velocity Verlet integration is: ", timeVV
echo "Velocity for velocity Verlet integration is: ", velVV
