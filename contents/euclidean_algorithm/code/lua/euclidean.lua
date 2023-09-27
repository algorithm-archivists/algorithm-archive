local function euclid_sub(a, b)
  a = math.abs(a)
  b = math.abs(b)

  while a ~= b do
    if a > b then
      a = a-b
    else
      b = b-a
    end
  end

  return a
end

local function euclid_mod(a, b)
  a = math.abs(a)
  b = math.abs(b)

  while b ~= 0 do
    a, b = b, a%b
  end

  return a
end

local function main()
  print("[#]\nModulus-based euclidean algorithm result:")
  print(euclid_mod(64 * 67, 64 * 81))
  print("[#]\nSubtraction-based euclidean algorithm result:")
  print(euclid_sub(128 * 12, 128 * 77))
end

main()
