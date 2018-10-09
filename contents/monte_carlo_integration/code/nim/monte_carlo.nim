import random
import math

randomize()

proc in_circle(x, y, radius: float): bool =
  return x * x + y * y < radius * radius

proc monte_carlo(samples: int): float =
  const radius: float = 1
  var count: int = 0

  for i in 0 .. < samples:
    let
      x: float = random(radius)
      y: float = random(radius)
        
    if in_circle(x, y, radius):
      count += 1
    
  let pi_estimate: float = 4 * count / samples
  return pi_estimate

let estimate: float = monte_carlo(1000000)

echo "the estimate of pi is ", estimate
echo "percent error: ", 100 * (abs(estimate - PI)/PI)