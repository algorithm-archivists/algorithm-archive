import Foundation


func isSorted(inputArray: [Int]) -> Bool {

    for i in 0..<inputArray.count-1 {
        if inputArray[i] > inputArray[i+1] {
            return false
        }
    }

    return true
}



func shuffle(inputArray: inout [Int]) -> [Int] {
    
    var shuffledArray = [Int]()

    for _ in 0..<inputArray.count {
        let rand = Int(arc4random_uniform(UInt32(inputArray.count)))
        shuffledArray.append(inputArray[rand])
        inputArray.remove(at: rand)
    }

    return shuffledArray
}



func bogoSort(sortArray: inout [Int]) -> [Int] {

    while(!isSorted(inputArray: sortArray)) {
        sortArray = shuffle(inputArray: &sortArray)
    }

    return sortArray
}
