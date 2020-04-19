proc print_array(a: openArray[int]) =
  for n in 0 .. < len(a):
    echo a[n]

proc bubble_sort(a: var openArray[int]) =
  for i in 0 .. < len(a) - 1:
    for j in 0 .. < len(a) - 1:
      if a[j + 1] < a[j]:
        swap(a[j], a[j + 1])

var x: array[10,int] =  [32, 32, 64, 16, 128, 8, 256, 4, 512, 2]
echo "Unsorted:"
print_array(x)
echo "\nSorted:"
bubble_sort(x)
print_array(x)
