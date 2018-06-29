import random


def merge(l1, l2):
    i = j = 0
    l = []

    while (i < len(l1)) and (j < len(l2)):
        if l1[i] < l2[j]:
            l.append(l1[i])
            i += 1
        else:
            l.append(l2[j])
            j += 1

    l.extend(l1[i:len(l1)])
    l.extend(l2[j:len(l2)])

    return l


def merge_sort(l):
    if len(l) == 1:
        return l

    l1 = merge_sort(l[0:len(l)//2])
    l2 = merge_sort(l[len(l)//2:len(l)])

    return merge(l1, l2)


def main():
    number = [random.randint(0, 10000) for _ in range(10)]
    print("Before Sorting  {}".format(number))
    number = merge_sort(number)
    print("After Sorting  {}".format(number))


if __name__ == "__main__":
    main()
