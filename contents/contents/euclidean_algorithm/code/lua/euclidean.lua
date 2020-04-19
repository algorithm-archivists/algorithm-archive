function euclidSub (a, b)
  local a = math.abs(a)
  local b = math.abs(b)

  while a ~= b do
    if a > b then
      a = a-b
    else
      b = b-a
    end
  end

  return a
end

function euclidMod (a, b)
  local a = math.abs(a)
  local b = math.abs(b)

  while b ~= 0 do
    a, b = b, a%b
  end

  return a
end

function main ()
  print(euclidSub(128 * 12, 128 * 77))
  print(euclidMod(64 * 67, 64 * 81))
end

main()
