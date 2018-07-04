func euclidSub(a: Int, b: Int) -> Int {
    var A = abs(a)
    var B = abs(b)
    
    while (A != B) {
        if (A > B) {
            A -= B
        } else {
            B -= A
        }
    }
    
    return A
}


func euclidMod(a: Int, b: Int) -> Int {
    var A = abs(a);
    var B = abs(b);
    
    while (B != 0) {
        let temp = B
        B = A % B
        A = temp
    }
    
    return A
}




func main() {
    print(euclidMod(a: 64 * 67, b: 64 * 81))
    print(euclidSub(a: 128 * 12, b: 128 * 77))
}


main()
