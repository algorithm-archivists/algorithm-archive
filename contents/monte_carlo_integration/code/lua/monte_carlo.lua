-- function to determine whether an x, y point is in the unit circle
local function in_circle(x, y)
  return x*x + y*y < 1
end

-- function to integrate a unit circle to find pi via monte_carlo
function monte_carlo(nsamples)
  local count = 0

  for i = 1,nsamples do
    if in_circle(math.random(), math.random()) then
      count = count + 1
    end
  end

  -- This is using a quarter of the unit sphere in a 1x1 box.
  -- The formula is pi = (box_length^2 / radius^2) * (pi_count / n), but we
  -- are only using the upper quadrant and the unit circle, so we can use
  -- 4*pi_count/n instead
  return 4 * count/nsamples
end

local pi = monte_carlo(10000000)
print("Estimate: " .. pi)
print(("Error: %.2f%%"):format(100*math.abs(pi-math.pi)/math.pi))
