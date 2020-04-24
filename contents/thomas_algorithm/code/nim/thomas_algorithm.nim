proc thomas_algorithm(a, b, c_in, d_in: seq[float]): seq[float] = 

  let n: int = len(d_in)

  var c: seq[float] = c_in
  var d: seq[float] = d_in
    
  c[0] /= b[0]
  d[0] /= b[0]

  for i in 1..n - 1:
    let scale: float = (1 / (b[i] - c[i - 1] * a[i]))

    c[i] *= scale
    d[i] = (d[i] - a[i] * d[i - 1]) * scale

  for i in countdown(n - 2,0):
    d[i] -= c[i] * d[i + 1]

  
  return d
            

const x: seq[float] = @[0.0, 2.0, 3.0]
const y: seq[float] = @[1.0, 3.0, 6.0]
const z: seq[float] = @[4.0, 5.0, 0.0]
const w: seq[float] = @[7.0, 5.0, 3.0]            
            
echo "The system,"
echo "[1.0 4.0 0.0][x] = [7.0]"
echo "[2.0 3.0 5.0][y] = [5.0]"
echo "[0.0 3.0 6.0][z] = [3.0]"

echo "has the solution:"

const soln: seq[float] = thomas_algorithm(x, y, z, w)

for i in 0..len(w) - 1:
  echo soln[i]
