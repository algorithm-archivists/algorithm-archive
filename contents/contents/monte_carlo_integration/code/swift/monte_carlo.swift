func inCircle(x: Double, y: Double, radius: Double) -> Bool {
    return (x*x) + (y*y) < radius*radius
}

func monteCarlo(n: Int) -> Double {
    let radius: Double = 1
    var piCount = 0
    var randX: Double
    var randY: Double

    for _ in 0...n {
        randX = Double.random(in: 0..<radius)
        randY = Double.random(in: 0..<radius)

        if(inCircle(x: randX, y: randY, radius: radius)) {
            piCount += 1
        }
    }

    let piEstimate = Double(4 * piCount)/(Double(n))
    return piEstimate
}

func main() {
    let piEstimate = monteCarlo(n: 10000)
    print("Pi estimate is: ", piEstimate)
    print("Percent error is: \(100 * abs(piEstimate - Double.pi)/Double.pi)%")
}

main()
