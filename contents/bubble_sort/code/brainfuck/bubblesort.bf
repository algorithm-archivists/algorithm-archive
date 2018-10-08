make some extra space for a marker
>>>

build array
>>++++
>>++++++
>>+++++
>>+++
>>+++++++
>>+
>>+++
>>++ 

move to starting point
>>>

set loop marker to 1 and starts loop
+
[

delete loop marker
-<<<<<

if there is a number here
[

add it to the empty space to the right 
and subtract it from the next number
[->+>-<<]

undo subtraction once without a zero check in case the numbers are equal
+>>+<-

then as long as the left number is bigger than zero
[<+>>

if the number to the right becomes a zero in the process 
due to buffer overflow set a swap marker
[<<<]<<[<<<<<+>>]

otherwise continue resetting numbers
>>>>>+<-]

set a "correct" marker between numbers
+

check swap marker
<<<<<<

if swap marker is set delete "correct" marker
[->>>>>>->

and swap the numbers
[-<+>]<<[->>+<<]>[-<+>]

go to next pair
<<<<<<]>>>

repeat until end of array
]

go to leftmost "correct" marker
>>>

delete marker and jump to the next one if it's 1
[->>]

else delete all markers and set repetition marker
>[>[-]>>+<]>]

program stops three places to the right of the 
sorted array
