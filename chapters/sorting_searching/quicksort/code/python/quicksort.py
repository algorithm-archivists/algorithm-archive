# submitted by KerimovEmil
import random


def quick_sort(array):
    ls_lower = []
    ls_upper = []
    ls_pivot = []
    if len(array) <= 1:
        return array
    else:
        pivot = array[0]  # pick any element from array
        for i in array:  # for each element in array, place in one of 3 lists
            if i < pivot:
                ls_lower.append(i)
            elif i > pivot:
                ls_upper.append(i)
            else:
                ls_pivot.append(i)
        ls_lower = quick_sort(ls_lower)
        ls_upper = quick_sort(ls_upper)
        return ls_lower + ls_pivot + ls_upper


if __name__ == '__main__':
    a = [random.randint(-100, 100) for _ in range(10)]
    print("Before Sorting  {}".format(a))
    a = quick_sort(a)
    print("After Sorting  {}".format(a))
