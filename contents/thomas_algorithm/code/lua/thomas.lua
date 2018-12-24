function thomas(a, b, c, d)

  -- Create tables and set initial elements
  local c_prime = {c[1] / b[1]}
  local result = {d[1] / b[1]}

  for i = 2, #a do
    -- Scale factor is for c_prime and result
    local scale = 1.0 / (b[i] - a[i] * c_prime[i - 1])
    c_prime[i] = c[i] * scale
    result[i] = (d[i] - a[i] * result[i - 1]) * scale
  end

  -- Back-substitution
  for i = #a-1, 1, -1 do
    result[i] = result[i] - (c_prime[i] * result [i + 1])
  end

  return result
end

local a = {0.0, 2.0, 3.0}
local b = {1.0, 3.0, 6.0}
local c = {4.0, 5.0, 0.0}
local d = {7.0, 5.0, 3.0}

print("The system")
print(b[1], c[1], "",   "|", d[1])
print(a[2], b[2], c[2], "|", d[2])
print("",   a[3], b[3], "|", d[3])
print("Has the solution:")

local solution = thomas(a, b, c, d)

print(table.unpack(solution))
