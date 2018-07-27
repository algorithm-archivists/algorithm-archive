import Foundation

func solveEuler(timeStep: Double, n: Int) -> [Double] {
    var result : [Double] = [1]

    for i in 1...n {
        result.append(result[i - 1] - 3 * result[i - 1] * timeStep)
    }
    
    return result
}

func checkResult(result: [Double], threshold: Double, timeStep: Double) -> Bool {
    var isApprox = true
    
    for i in 0..<result.count {
        let solution = exp(-3 * Double(i) * timeStep)
        if abs(result[i] - solution) > threshold {
            print(result[i], solution)
            isApprox = false
        }
    }
    
    return isApprox
}

func main() {
    let timeStep = 0.01
    let n = 100
    let threshold = 0.01

    let result = solveEuler(timeStep: timeStep, n: n)
    let isApprox = checkResult(result: result, threshold: threshold, timeStep: timeStep)

    if isApprox {
        print("All values within threshold")
    } else {
        print("Value(s) not in threshold")
    }
}

main()
