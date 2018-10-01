>,>,   scan
[      while b!=0
0 a (b) 0 0 0

<[
   >->+<[>]
   >[<+>-]<
   <[<]>-
]
so basically this is the mod algo in brainfuck, it slowly shifts cell 2 to cell 3, while subtracting 1 from cell 1
then when cell 2 goes to 0, it shifts cell 3 to 2 and continues, this is like just constantly subtracting cell 2 from cell 1, until you cant subtract anymore then return at cell 3
0 (0) b_a%b a%b 0 0

>[-<+>]
0 b_a%b (0) a%b 0 0

>[-<+<+>>]<
0 b (a%b) 0 0 0

]
0 GCD(a b) 0 0 0 0

<.
