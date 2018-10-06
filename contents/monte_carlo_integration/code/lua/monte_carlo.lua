local function in_circle(x, y)
  return x*x + y*y <= 1
end

function monte_carlo(nsamples)
  local count = 0
  
  for i = 1,nsamples do
    if in_circle(math.random(), math.random()) then
      count = count + 1
    end
  end
  
  return 4 * count/nsamples
end

local pi = monte_carlo(10000000)
print("Estimate: " .. pi)
print(("Error: %.2f%%"):format(100*math.abs(pi-math.pi)/math.pi))
