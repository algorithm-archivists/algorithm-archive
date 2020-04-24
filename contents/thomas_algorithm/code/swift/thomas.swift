func thomas(a: [Double], b: [Double], c: [Double], d: [Double]) -> [Double] {
    var a = a
    var b = b
    var c = c
    var d = d

    // set the initial elements
    c[0] = c[0] / b[0]
    d[0] = d[0] / b[0]

    let n = d.count // number of equations to solve
    for i in 1..<n {
        // scale factor for c and d
        let scale = 1 / (b[i] - c[i-1] * a[i])

        c[i] = c[i] * scale
        d[i] = (d[i] - a[i] * d[i-1]) * scale
    }

    // do the back substitution
    for i in stride(from: n-2, to: -1, by: -1) {
        d[i] = d[i] - c[i] * d[i+1]
    }

    return d
}

func main() {
    let a = [0.0, 2.0, 3.0]
    let b = [1.0, 3.0, 6.0]
    let c = [4.0, 5.0, 0.0]
    let d = [7.0, 5.0, 3.0]

    print(thomas(a: a, b: b, c: c, d: d))
}

main()
