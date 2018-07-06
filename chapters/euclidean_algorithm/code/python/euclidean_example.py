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
            a -= b
        else:
            b -= a

    return a

if __name__=="__main__":
    print('Eucledean Mod: {}\nEucledean sub: {}'.format(euclid_mod(64 * 67, 64 * 81), euclid_sub(128 * 12, 128 * 77)))
