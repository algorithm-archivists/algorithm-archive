func euclid_mod(in1, in2: int): int =
  var
    a = abs(in1)
    b = abs(in2)

  while b != 0:
    let temp: int = b
    b = a mod b
    a = temp;

  result = a

func euclid_sub(in1, in2: int): int =
  var
    a = abs(in1)
    b = abs(in2)

  while a != b:
    if a > b:
      a -= b
    else:
      b -= a

  result = a

when isMainModule:
  echo "[#]\nModulus-based euclidean algorithm result:"
  echo euclid_sub(64 * 67, 64 * 81)
  echo "[#]\nSubtraction-based euclidean algorithm result:"
  echo euclid_mod(128 * 12, 128 * 77)
