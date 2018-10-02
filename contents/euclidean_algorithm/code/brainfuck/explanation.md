Pseudocode:
```
word a,b;
scan a,b;
while(b!=0){
    a,b=b,a%b;
}
return a;
```

`scan a,b`: `>,>,`

State: `0 a >b 0 0 0`

`while(b!=0)`: `[`

`a,b,0=0,b-a%b,a%b`:
```
<[
   >->+<[>]
   >[<+>-]<
   <[<]>-
]
```

so basically this is the modulo algorithm in brainfuck, it slowly shifts cell 2 to cell 3, while subtracting 1 from cell 1
then when cell 2 goes to 0, it shifts cell 3 to 2 and continues, this is like just constantly subtracting cell 2 from cell 1, until you cant subtract anymore then return at cell 3

State: `0 >0 b-a%b a%b 0 0`

shifting: `>[-<+>]`

State: `0 b-a%b >0 a%b 0 0`

Currently we have a,b,0=b-a%b,0,a%b, and we need a,b,0=b,a%b,0, so we just add the third cell to the first and second cell

adding thing: `>[-<+<+>>]<`

State: `0 b >(a%b) 0 0 0`

So now we have a,b=b,a%b, we continue the loop

`]`

After the second cell is 0, the loop terminates and we obtain the GCD

State: `0 >GCD(a b) 0 0 0 0`

Now we print the GCD

print: `<.`
