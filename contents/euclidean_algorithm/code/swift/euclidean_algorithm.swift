func euclidSub(a: Int, b: Int) -> Int {
    var a = abs(a)
    var b = abs(b)
    
    while (a != b) {
        if (a > b) {
            a -= b
        } else {
            b -= a
        }
    }
    
    return a
}

func euclidMod(a: Int, b: Int) -> Int {
    var a = abs(a);
    var b = abs(b);
    
    while (b != 0) {
        let temp = b
        b = a % b
        a = temp
    }
    
    return a
}

func main() {
    print(euclidMod(a: 64 * 67, b: 64 * 81))
    print(euclidSub(a: 128 * 12, b: 128 * 77))
}

main()
