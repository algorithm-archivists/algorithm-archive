we need some extra space in the beginning of the memory
because we need to put a marker in the first memory location
and not all brainfuck compilers have infinite memory
>>>

here we build the array
we leave an empty space between all the numbers so we have space to 
do operations if we need to
>>++++
>>++++++
>>+++++
>>+++
>>+++++++
>>+
>>+++
>>++ 

we need to move three places to the right because this is the ending point
of the main loop
>>>

set the memory loation to 1 and start the loop 
+
[

delete the one that tells us we're not finished and move to the 
second to last number in the array
-<<<<<

if there is a number here
[

add it to the empty space to the right and subtract it 
from the next number
[->+>-<<]

now we undo or last step
>

once without a zero check in case the numbers are equal
<+>>+<-

then as long as the left number is bigger than zero
[<+>>

but if the number to the right becomes a zero in the process 
(this happens due to a so called "buffer overflow" where the 
memory location wraps around to zero if we put more values in 
than it can hold) we go to the next memory location to the 
left that we don't want to be zero and set it to one as a marker 
that we need to swap the numbers
[<<<]<<[<<<<<+>>]

otherwise we just continue on with resetting the numbers
>>>>>+<-]

then we put a one inbetween the numbers to mark that they have not
been swapped
+

now we go and check the memory location that we used to memorize
whether the number to the right was zero (thus making it smaller than
the one on the left)
<<<<<<

if this memory location is set to one we undo our marker that the 
numbers have not been swapped
[->>>>>>->

and swap the numbers
[-<+>]<<[->>+<<]>[-<+>]

then we go to the place of the left number of the next pair to check
<<<<<<]>>>

and see if there is a numer there
]

if there is none we go to the place of the leftmost switch marker
>>>

and delete the marker and jump to the next one if it's 1
[->>]

or delete all of them and set the repetition marker on the right to one
>[>[-]>>+<]>]

after all this is done the program stops three places to the right
of the rightmost number in the now sprted array
