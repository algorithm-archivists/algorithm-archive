proc euclid_mod(in1, in2: int): int =
  var
    a = abs(in1)
    b = abs(in2)
    
  while b != 0:
    let temp: int = b
    b = a mod b
    a = temp;

    return a

proc euclid_sub(in1, in2: int): int =
  var
    a = abs(in1)
    b = abs(in2)

  while a != b:
    if a > b:
      a -= b
    else:
      b -= a
    
    return a

echo euclid_sub(32*5, 32*3)
echo euclid_mod(64*2, 64*7)
