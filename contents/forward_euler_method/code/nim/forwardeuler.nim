import math

proc solveEuler(timestep: float, n: int): seq[float] =
  var res = newSeq[float](n)
  
  res[0] = 1.0

  for i in 1 .. n - 1:
    res[i] = res[i - 1] - 3 * res[i - 1] * timestep

  return res

proc check(res: seq[float], timestep, threshold: float): bool =
  var approx: bool = true;

  for i in 0 .. len(res) - 1:
    let solution: float = exp(-3.0 * float(i) * timestep)
    if abs(res[i]) - solution > threshold:
      echo res[i]
      echo solution
      approx = false

  return approx

const
  timestep: float = 0.1
  n: int = 100
  threshold: float = 0.1
  euler_result: seq[float] = solve_euler(timestep, n)
  approx: bool = check(euler_result, threshold, timestep)

echo approx
