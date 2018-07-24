//Double Extension from YannickSteph on StackOverflow: https://stackoverflow.com/questions/25050309/swift-random-float-between-0-and-1

import Foundation

public extension Double {
    public static var random: Double {
        return Double(arc4random()) / 0xFFFFFFFF // Returns a random double between 0.0 and 1.0, inclusive.
    }
    
    public static func random(min: Double, max: Double) -> Double {
        return Double.random * (max - min) + min
    }
}

func isInCircle(x: Double, y: Double, radius: Double) -> Bool {
    return (x*x) + (y*y) < radius*radius
}

func monteCarlo(n: Int) -> Double {
    let radius: Double = 1
    var piCount = 0
    var randX: Double
    var randY: Double
    
    for _ in 0...n {
        randX = Double.random(min: 0, max: radius)
        randY = Double.random(min: 0, max: radius)
        
        if(isInCircle(x: randX, y: randY, radius: radius)) {
            piCount += 1
        }
    }
    
    let piEstimate = Double(4 * piCount)/(Double(n))
    return piEstimate
}

func main() {
    let piEstimate = monteCarlo(n: 10000)
    print("Pi estimate is: ", piEstimate)
    print("Percent error is: \(100*(Double.pi - piEstimate)/Double.pi)%")
}


main()

