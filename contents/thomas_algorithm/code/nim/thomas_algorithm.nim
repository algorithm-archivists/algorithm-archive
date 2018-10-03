const x: array[3,float] = [0.0,2.0,3.0]
const y: array[3,float] = [1.0,3.0,6.0]
const z: array[3,float] = [4.0,5.0,0.0]
const w: array[3,float] = [7.0,5.0,3.0]

const n: int = len(w)

proc thomas_algorithm(a,b,c_in,d_in: array[n,float]): array[n,float] = 

    #const n: int = len(d1)

    var c: array[n,float] = c_in
    var d: array[n,float] = d_in
    
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    for i in 1..n-1:
        let scale: float = (1 / (b[i] - c[i-1] * a[i]))

        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i - 1]) * scale

    for i in countdown(n-2,0):
        d[i] -= c[i]*d[i+1]

    return d
            
            
echo "The system,"
echo "[1.0 4.0 0.0][x] = [7.0]"
echo "[2.0 3.0 5.0][x] = [5.0]"
echo "[0.0 3.0 6.0][x] = [3.0]"

echo "has the solution:"

const soln: array[3,float] = thomas_algorithm(x,y,z,w)

for i in 0..2:
    echo soln[i]
