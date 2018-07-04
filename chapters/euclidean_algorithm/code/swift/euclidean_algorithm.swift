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
    print(euclidSub(a: 24, b: 27))
    print(euclidMod(a: 40, b: 252))
}


main()
