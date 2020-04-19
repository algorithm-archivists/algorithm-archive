import random

randomize()

proc print_array(a: openArray[int]) =
    for n in 0 .. len(a)-1:
        echo a[n]

proc is_sorted(a: openArray[int]): bool =
    for n in 1 .. len(a)-1:
        if a[n] > a[n-1]:
            return false
    
    return true

proc bogo_sort(a: var openArray[int]) =
    while not is_sorted(a):
        shuffle(a)
        

var x: array[10,int] =  [32,32,64,16,128,8,256,4,512,2]

print_array(x)
bogo_sort(x)
echo "\n"
print_array(x)
