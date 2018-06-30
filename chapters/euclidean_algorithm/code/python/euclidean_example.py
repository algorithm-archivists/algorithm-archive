# submitted by KerimovEmil
def euclid_mod(a, b):
    a = abs(a)
    b = abs(b)

    while b > 0:
        a, b = b, a % b

    return a


def euclid_sub(a, b):
    a = abs(a)
    b = abs(b)

    while a != b:
        if a > b:
            a = a - b
        else:
            b = b - a

    return a


print(euclid_mod(64 * 67, 64 * 81))
print(euclid_sub(128 * 12, 128 * 77))
