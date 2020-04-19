import std.stdio;
import std.math;

// Euclidean algorithm using modulus
int euclid_mod(int a, int b) {
    int tmp;
    a = abs(a);
    b = abs(b);

    while (b != 0) {
        tmp = a % b;
        a = b;
        b = tmp;
    }

    return a;
}

// Euclidean algorithm with subtraction
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

void main()
{
    auto check1 = euclid_mod(64 * 67, 64 * 81);
    auto check2 = euclid_sub(128 * 12, 128 * 77);

    writeln("Modulus-based euclidean algorithm result: ", check1);
    writeln("Subtraction-based euclidean algorithm result: ", check2);
}
