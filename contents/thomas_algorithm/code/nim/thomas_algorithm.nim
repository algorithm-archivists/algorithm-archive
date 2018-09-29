proc thomas_algorithm(a,b,c,d: var array[3,float]) = 
    
    let n: int = len(d)
    
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    for i in 1..n-1:
        let scale: float = (1 / (b[i] - c[i-1] * a[i]))

        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i - 1]) * scale

    for i in countdown(n-2,0):
        d[i] -= c[i]*d[i+1]
            


var x: array[3,float] = [0.0,2.0,3.0]
var y: array[3,float] = [1.0,3.0,6.0]
var z: array[3,float] = [4.0,5.0,0.0]
var w: array[3,float] = [7.0,5.0,3.0]

echo "The system,"
echo "[1.0 4.0 0.0][x] = [7.0]"
echo "[2.0 3.0 5.0][x] = [5.0]"
echo "[0.0 3.0 6.0][x] = [3.0]"

echo "has the solution:"

thomas_algorithm(x,y,z,w)

for i in 0..2:
    echo w[i]
