import random


def bubble_sort(array):
    len_array = len(array)
    for i in range(len_array):
        for j in range(len_array - i - 1):
            if(array[j] > array[j+1]):
                array[j], array[j+1] = array[j+1], array[j] #swap elements in the list

def main():
    random_array = [random.randint(0, 1000) for _ in range(10)]
    print("Before Sorting  {}".format(random_array))
    bubble_sort(random_array)
    print("After Sorting  {}".format(random_array))

main()
