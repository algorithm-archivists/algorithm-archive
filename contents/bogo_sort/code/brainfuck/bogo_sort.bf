rng summand
+++++++++

rng seed 1
>>>,+++

rng seed 2
>+++++++

leave space for random function
>>>>>>>

leave space fur swapping numbers
>>>>>>>

array
+>
+++++++
>>>

+>
++++++
>>>

+>
+++++++++++++++++
>>>

+>
++
>>>

+>

>>>

+>
+++++++++++++++++++++++++++++++++++++
>>>

go to first element marker
<<<<[<<<<]>>>>

count array
[<+[->>>>+<<<<]>>>>>]<<<<[>>>[-<<<<+>>>>]<<<<<<<]>>>[-<<+>>]

go to first el marer again
>

=== check if the array is sorted ===
if marker is present
[

go to next marker
>>>>

if next marker is also present
[

go to current number
<<<

if number is bigger than zero but the next one
is not set unordered marker
[>>>>[<<]<<<<<[<+<]]

go back to current number
>>[>>>]<<

add it to the empty space to the right 
and subtract it from the next number
[->+>>>-<<<<]

undo subtraction once without a zero check in case the numbers are equal
>[-<+>>>>+<<]

make sure pointer stays the same even if values are 0
>[<]<

then as long as the left number is bigger than zero
[<+>>>>

if the number to the right becomes a zero in the process 
due to buffer overflow set a unordered marker
[<<]<<<<<[<+<]

otherwise continue resetting numbers
>>>>>>>+<<<-]

move unordered marker and set it to one if bigger
<<<[>>>>+<<<<[-]]>>>

break when check is done
]

go to next array element marker
>>

repeat until end of array
]

move unordered marker to beginning of array
<<<<<<[<[-<<<<+>>>>]<<<]

go to marker
<
=== end of sorted check ===

if array is not sorted
[-

make two copies of array length
>>[-<+<<+>>>]<[->+<]

for each element in the array
<<[

=== random number generator ===
go to seed 2 of rng
<<<<<<<<

check if its 0
if so add 2
>>++<<[>]>>[-<<+>>]<[[-]<]<

make two copies
[->+<<<+>>]

go to seed 1(n)
<

make two copies
[-<+>>>>>+<<<<]

go to copy of seed 2(m)
>>

sprinkle in more randomness
-

=== beginning of n mod m ===

as long as pointer is on m (and not
an empty field) at this position
[

copy m twice
[->+>>+<<<]

and put one copy back in the original place
>>>[-<<<+>>>]

go to the second copy
<<

subtract it from n
[->-

for each step copy the remainder twice
[->+>+<<]

bring back one copy
>>[-<<+>>]

go to the second copy which is the marker for n
being zeroed out in the process
<

if the marker is set
[

delete the marker
[-]

go to the working copy of m
<<

move it right two places
[->>+<<]

exit marker check loop
]

check for value (first m which should be set
then n to do the zero check and displace the pointer
<[

go to working copy of m
or an empty cell
>>>

move it back
[-<<+>>]

go right and check for repeat
>]

go back to working copy of m
or empty cell if n is 0
<<<

repeat until working copy is 0
]

go back to m (or another empty cell if n is 0
<

repeat until pointer is on empty cell
]

go to the rest of the working copy of m
>>>

subtract it from m and make a copy
[-<-<+>>]

go to the copy
<<

if the copy is nonzero
[

delete copy
[-]

copy over subtraction result
>[->+<]

go to empty cell and break loop
<]

go to subtraction result position and delete it
(if its needed it was copied over otherwise result is 0)
>[-]

go to result
>
=== end of n mod m ===

make two copies of result
[-<+>>+<]

go to seed 1
<<<<

move it right
[->+<]

go to seed 2
>>>

move it right
[-<+>]

go to summand
<<<<<

coppy summand twice
thus uaading it to seed 1
[->+>>>+<<<<]

go to the first copy
>

move it left
[-<+>]

go to result of rng
>>>>>>
=== end of rng ===

move it
[->>>>+<<<<]

go to array pos ctr
>>>>>

make a copy
[->>+>>>>>>+<<<<<<<<]>>[-<<+>>]

go to copy
>>>>>>

move through array subtracting one each step
[[-[->>>>+<<<<]]>>>>]<<<<

move number 1 to empty cell
<[->+<]

move it to the left end of the array
<[>>[-<<<<+>>>>]<<<<<<]

move num 1 to a safe place
>>>>>>[-<<<<+>>>>]

go to rng result
<<<<<<<<<

move it
[->>>>>>>>>+<<<<<<<<<]

go to copy
>>>>>>>>>
for a random number of steps
[>>

go right as far as possible
[<<[-[->>>>+<<<<]]>>>>[>]>]

bring case pointers together
<<<<[<]<<<

go left as far as possible
[>>>>>>[-[-<<<<+>>>>]]<<<<[<<<<<<<]>]

bring case pointers together
>>>>[>>>>>>>]<<<<<

until at a random pos
]

go to array value
<

move number 2 to empty cell
[->+<]

move it to the left end of the array and count steps
<<<<<[>>>>>>[-<<<<+>>>>]>[-<<<<+>>>>]<<<<+<<<<<<<]

move step count to a safe space
>>>>>>>[-<<<<<<<<<<+>>>>>>>>>>]

go to num 1 index
<<<<<<<<<

make a copy
[->>+>>>>>>>+<<<<<<<<<]>>[-<<+>>]

go to copy
>>>>>>>

move through array subtracting one each step
and take number 2 with us
[[-[->>>>+<<<<]<[->>>>+<<<<]]>>>>>]

put num 2 in array
<[-<<<<<+>>>>>]

go to left end of array
<<<<<<[<<<<]

move number 1 in position
>>[->>>>+<<<<]

go to num 2 index
<<<<<

move it into place
[->>>>>>>>>>+<<<<<<<<<<]

go to copy
>>>>>>>>>>

move through array subtracting one each step
and take number 1 with us
[[-[->>>>+<<<<]<[->>>>+<<<<]]>>>>>]

put num 1 in array
<[-<+>]

go to left end of array
<<[<<<<]

go to remaining indices
<<#

repeat for all indices
-]

go to array element marker
>>>>>>

=== check if the array is sorted ===
if marker is present
[

go to next marker
>>>>

if next marker is also present
[

go to current number
<<<

if number is bigger than zero but the next one
is not set unordered marker
[>>>>[<<]<<<<<[<+<]]

go back to current number
>>[>>>]<<

add it to the empty space to the right
and subtract it from the next number
[->+>>>-<<<<]

undo subtraction once without a zero check in case the numbers are equal
>[-<+>>>>+<<]

make sure pointer stays the same even if values are 0
>[<]<

then as long as the left number is bigger than zero
[<+>>>>

if the number to the right becomes a zero in the process
due to buffer overflow set a unordered marker
[<<]<<<<<[<+<]

otherwise continue resetting numbers
>>>>>>>+<<<-]

move unordered marker and set it to one if bigger
<<<[>>>>+<<<<[-]]>>>

break when check is done
]

go to next array element marker
>>

repeat until end of array
]

move unordered marker to beginning of array
<<<<<<[<[-<<<<+>>>>]<<<]

go to marker
<
=== end of sorted check ===

repeat until sorted
]

go to beginning of array
>>>>>

print array
[>++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------>>>]
