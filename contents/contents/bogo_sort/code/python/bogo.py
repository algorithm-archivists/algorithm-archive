import random


def is_sorted(a):
    for i in range(len(a)-1):
        if a[i+1] < a[i]:
            return False
    return True

def bogo_sort(a):
    while not is_sorted(a):
        random.shuffle(a)

def main():
    a = [1, 3, 2, 4]
    bogo_sort(a)
    print(a)

main()

