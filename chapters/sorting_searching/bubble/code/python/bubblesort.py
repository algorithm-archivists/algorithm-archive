#!usr/bin/python3

#import section 
import random


def bubble_sort(array):
  len_array = len(array)
  for i in range(len_array):
    for j in range(len_array - i - 1):
      if(array[j] > array[j+1]):
        array[j], array[j+1] = array[j+1], array[j] #swap elements in the list


def main():
  number = [random.randint(0, 10000) for _ in range(10)]
  print("Before Sorting  {}".format(number))
  bubble_sort(number)
  print("After Sorting  {}".format(number))


if __name__ == "__main__":
  main()
