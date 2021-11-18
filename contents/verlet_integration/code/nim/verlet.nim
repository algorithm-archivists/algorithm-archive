func verlet(pos_in, acc, dt: float): float =
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

  time

func stormerVerlet(pos_in, acc, dt: float): (float, float) =
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

  (time, vel)

func velocityVerlet(pos_in, acc, dt: float): (float, float) =
  var
    pos: float = pos_in
    time: float = 0.0
    vel: float = 0.0

  while pos > 0.0:
    time += dt
    pos += vel * dt + 0.5 * acc * dt * dt
    vel += acc * dt

  (time, vel)

when isMainModule:
  let timeV = verlet(5.0, -10.0, 0.01)
  echo "[#]\nTime for Verlet integration is:"
  echo timeV

  let (timeSV, velSV) = stormerVerlet(5.0, -10.0, 0.01)
  echo "[#]\nTime for Stormer Verlet integration is:"
  echo timeSV
  echo "[#]\nVelocity for Stormer Verlet integration is:"
  echo velSV

  let (timeVV, velVV) = velocityVerlet(5.0, -10.0, 0.01)
  echo "[#]\nTime for velocity Verlet integration is:"
  echo timeVV
  echo "[#]\nVelocity for velocity Verlet integration is:"
  echo velVV
