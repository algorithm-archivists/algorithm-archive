// sumbitted by lolatomroflsinnlos
public static void main(String[] args) {

    System.out.println(euclidSub(64 * 67, 64 * 81));
    System.out.println(euclidMod(128 * 12, 128 * 77));

}

public static int euclidSub(int a, int b) {
    a = Math.abs(a);
    b = Math.abs(b);

    while (a != b) {
        if (a > b) {
            a -=b;
        } else {
            b -=a;
        }
    }

    return a;
}

public static int euclidMod(int a, int b) {
    a = Math.abs(a);
    b = Math.abs(b);

    while (b != 0){
        int temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}

