#include <stdio.h>
#include <math.h>

int euclid_mod(int a, int b) {
    a = abs(a);
    b = abs(b);

    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}

int euclid_sub(int a, int b) {
    a = abs(a);
    b = abs(b);

    while (a != b) {
        if (a > b) {
            a -= b;
        } else {
            b -= a;
        }
    }

    return a;
}

int main() {
    int check1 = euclid_mod(64 * 67, 64 * 81);
    int check2 = euclid_sub(128 * 12, 128 * 77);

    printf("%d\n", check1);
    printf("%d\n", check2);

    return 0;
}
