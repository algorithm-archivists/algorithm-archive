func bubbleSort(sortArray: inout [Int]) -> [Int] {
    for i in (1..<sortArray.count).reversed() {
        for j in 0..<i {
            if sortArray[j] > sortArray[j+1] {
                let temp = sortArray[j]
                sortArray[j] = sortArray[j + 1]
                sortArray[j + 1] = temp
            }
        }
    }
    
    return sortArray
}

func main() {
    var testArray = [4,5,123,759,-132,8940,24,34,-5]
    print(bubbleSort(sortArray: &testArray))
}

main()
