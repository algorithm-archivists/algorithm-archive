import Foundation

func isSorted(inputArray: [Int]) -> Bool {
    for i in 0..<inputArray.count-1 {
        if inputArray[i] > inputArray[i+1] {
            return false
        }
    }

    return true
}

func bogoSort(sortArray: inout [Int]) -> [Int] {
    while(!isSorted(inputArray: sortArray)) {
        sortArray.shuffle()
    }

    return sortArray
}

func main() {
    var testArray = [4,5,123,24,34,-5]
    print(bogoSort(sortArray: &testArray))
}

main()
