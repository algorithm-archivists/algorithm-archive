proc verlet(pos_in, acc, dt: float): float =
  var
    pos: float = pos_in
    prev_pos: float = pos
    time: float = 0.0
    temp_pos: float

  while pos > 0.0:
    time += dt
    temp_pos = pos
    pos = pos * 2 - prev_pos + acc * dt * dt 
    prev_pos = temp_pos

  return time

proc stormer_verlet(pos_in, acc, dt: float): (float, float) =
  var
    pos: float = pos_in
    prev_pos: float = pos
    time: float = 0.0
    vel: float = 0.0
    temp_pos: float

  while pos > 0.0:
    time += dt
    temp_pos = pos
    pos = pos * 2 - prev_pos + acc * dt * dt
    prev_pos = temp_pos

    vel += acc * dt

  return (time, vel)

proc velocity_verlet(pos_in, acc, dt: float): (float, float) =
  var
    pos: float = pos_in
    time: float = 0.0
    vel: float = 0.0

  while pos > 0.0:
    time += dt
    pos += vel * dt + 0.5 * acc * dt * dt
    vel += acc * dt

  return (time, vel)

let time_v: float = verlet(5.0, -10.0, 0.01)
echo "Time for Verlet integration is: ", time_v

let (time_sv, vel_sv) = stormer_verlet(5.0, -10.0, 0.01)
echo "Time for Stormer Verlet integration is: ", time_sv
echo "Velocity for Stormer Verlet integration is: ", vel_sv

let (time_vv, vel_vv) = velocity_verlet(5.0, -10.0, 0.01)
echo "Time for velocity Verlet integration is: ", time_vv
echo "Velocity for velocity Verlet integration is: ", vel_vv