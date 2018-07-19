# Quicksort

Quicksort is the fastest algorithm we cover so far and its based on the stament "divide and conquer".
The best way to explain how this algorithm works is with a quick example, lets take the first 5 odd numbers in a random order.

[3,7,5,9,1]

The first step is to take a number (any number) and lets call it a "pivot". Then we proced to compare this number with every other number and decide for each one if it should be on the right or left side of out pivot.
Continuin with my example i will take the number 7 as my pivot and ones compared with overy other number i get:

[3,5,1] Numbers that should be on the left of my pivot
[7] Pivot
[9] Numbers that should be on the right of my pivot

Now we just need to repeat this procees for each new list (the one of numbers on the right and the one of number on the left) but since on the right i got only one number y i ll only continue with the left side.
For our list on the left [3,5,1] we chose a new random pivot like 3 and we compare it with the rest of the list.

[1] Numbers that should be on the left of my pivot
[3] Pivot
[5] Numbers that should be on the right of my pivot
[7] Pivot (1st iteration)
[9] Numbers that should be on the right of my pivot (1st iteration)

One all your lists only have one number left the only logical step left is to concat them resulting in the final expected result.

[1,3,5,7,9]
