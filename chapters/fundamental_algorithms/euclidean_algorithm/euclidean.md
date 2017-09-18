<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>
$$ 
\newcommand{\d}{\mathrm{d}}
\newcommand{\bff}{\boldsymbol{f}}
\newcommand{\bfg}{\boldsymbol{g}}
\newcommand{\bfp}{\boldsymbol{p}}
\newcommand{\bfq}{\boldsymbol{q}}
\newcommand{\bfx}{\boldsymbol{x}}
\newcommand{\bfu}{\boldsymbol{u}}
\newcommand{\bfv}{\boldsymbol{v}}
\newcommand{\bfA}{\boldsymbol{A}}
\newcommand{\bfB}{\boldsymbol{B}}
\newcommand{\bfC}{\boldsymbol{C}}
\newcommand{\bfM}{\boldsymbol{M}}
\newcommand{\bfJ}{\boldsymbol{J}}
\newcommand{\bfR}{\boldsymbol{R}}
\newcommand{\bfT}{\boldsymbol{T}}
\newcommand{\bfomega}{\boldsymbol{\omega}}
\newcommand{\bftau}{\boldsymbol{\tau}}
$$

# Euclidean Algorithm

Computer science is (almost by definition) a science about computers -- a device first conceptualized in the 1800's. Computers have become so revolutionary, that it is difficult to think of our lives today without them. That said, *algorithms* are much older and have existed in the world for millenia. Incredibly, a few of the algorithms created before the Common Era (AD) are still in use today. One such algorithm was first described in Euclid's *Elements* (~ 300 BC) and has come to be known as the *Euclidean Algorithm*. 

The algorithm is a simple way to find the *greatest common divisor* (GCD) of two numbers, which is useful for a number of different applications (like reducing fractions). The first method (envisioned by Euclid) uses simple subtraction:

```python
function euclid_sub(a::Int64, b::Int64)
    while (a != b)
        if (a > b)
            a = a - b
        else
            b = b - a
        end
    end
end
```

Here, we simply line the two numbers up every step and subtract the lower value from the higher one every timestep. Once the two values are equal, we call that value the greatest common divisor. A graph of `a` and `b` as they change every step would look something like this:

![Subtraction-based Euclidean algorithm](subtraction.png)

Modern implementations, though, often use the modulus operator (%) like so

```python
function euclid_mod(a::Int64, b::Int64)
    temp = Int64
    while (b != 0)
        temp = b
        b = a%b
        a = temp
    end
end
```

Here, we set `b` to be the remainder of `a%b` and `a` to be whatever `b` was last timestep. Because of how the modulus operator works, this will provide the same information as the subtraction-based implementation, but when we show `a` and `b` as they change with time, we can see that it might take many fewer steps:

![Modulus-based Euclidean algorithm](modulus.png)

The Euclidean Algorithm is truly fundamental to many other algorithms throughout the history of computer science and will definitely be used again later. At least to me, it's amazing how such an ancient algorithm can still have modern use and appeal. That said, there are still other algorithms out there that can find the greatest common divisor of two numbers that are arguably better in certain cases than the Euclidean algorithm, but the fact that we are discussing Euclid two millenia after his death shows how timeless and universal mathematics truly is. I think that's pretty cool.

# Example Code
### C++
```cpp
/*-------------euclidean.cpp--------------------------------------------------//
*
* Purpose: To implement euclidean algorithm to find the greatest common divisor
*
*   Notes: Compile with g++ euclidean.cpp
*
*-----------------------------------------------------------------------------*/

#include <iostream>
#include <cmath>

// Euclidean algorithm with mod
int euclid_mod(int a, int b){
    a = std::abs(a);
    b = std::abs(b);

    int temp;
    while (b != 0){
        temp = b;
        b = a%b;
        a = temp;
    }

    return a;
}

// Euclidean algorithm with subtraction
int euclid_sub(int a, int b){
    a = std::abs(a);
    b = std::abs(b);

    while (a != b){
        if (a > b){
            a = a - b;
        }
        else{
            b = b - a;
        }
    }

    return a;
}

int main(){

    int check = euclid_mod(64*67, 64*81);
    int check2 = euclid_sub(128*12, 128*77);

    std::cout << check << '\n';
    std::cout << check2 << '\n';
}

```

### C
```c
#include <stdio.h>
#include <stdlib.h>

int euclid_mod(int a, int b){
    a = abs(a);
    b = abs(b);

    int temp;
    while (b != 0){
        temp = b;
        b = a%b;
        a = temp;
    }

    return a;
}

int euclid_sub(int a, int b){
    a = abs(a);
    b = abs(b);

    while (a != b){
        if (a > b){
            a = a - b;
        }
        else{
            b = b - a;
        }
    }

    return a;
}

int main(){

    int check = euclid_mod(64*67, 64*81);
    int check2 = euclid_sub(128*12, 128*77);

    printf("%d\n", check);
    printf("%d\n", check2);
}

```

### JavaScript

```html
<!DOCTYPE html>
<html>
<body>
<script>
function euclid_mod(a, b){
    a = Math.abs(a);
    b = Math.abs(b);

    var temp;
    while (b != 0){
        temp = b;
        b = a%b;
        a = temp;
    }

    return a;
}

function euclid_sub(a, b){
    a = Math.abs(a);
    b = Math.abs(b);

    while (a != b){
        if (a > b){
            a = a - b;
        }
        else{
            b = b - a;
        }
    }

    return a;
}

document.write(euclid_mod(64*67, 64*81) + "<br>");
document.write(euclid_sub(128*12, 128*77) + "<br>");
</script>
</body>
</html>
```

### Python

```python

#  euclidean.py

def euclid_mod(a, b):

    temp = 0

    while b > 0:
        temp = b
        b = a % b
        a = temp

    return a

def euclid_sub(a, b):

    while a != b:
        if a > b:
            a = a - b
        else:
            b = b - a

    return a

print euclid_mod(64 * 67, 64 * 81)
print euclid_sub(128 * 12, 128 * 77)
```

### C#

```cs
// submitted by Julian Schacher‏
using System;

namespace Euclidean_Algorithm
{
    class Program
    {
        static void Main(string[] args)
        {
            int check = Algorithms.EuclidMod(64 * 67, 64 * 81);
            int check2 = Algorithms.EuclidSub(128 * 12, 128 * 77);

            Console.WriteLine(check);
            Console.WriteLine(check2);
        }
    }

    public static class Algorithms
    {
        public static int EuclidSub(int a, int b)
        {
            while (a != b)
            {
                if (a > b)
                    a = a - b;
                else
                    b = b - a;
            }

            return a;
        }

        public static int EuclidMod(int a, int b)
        {
            while (b != 0)
            {
                var temp = b;
                b = a % b;
                a = temp;
            }

            return a;
        }
    }
}
```

### Haskell

```haskell
euclidSub :: Integer -> Integer -> Integer
euclidSub a b =
  if a == b then
    a
  else if a < b then
    euclidSub a (b - a)
  else
    euclidSub (a - b) b

euclidMod :: Integer -> Integer -> Integer
euclidMod a 0 = a
euclidMod a b = euclidMod b (a `mod` b)

main :: IO ()
main = do
  let chk1 = euclidMod (64 * 67) (64 * 81)
      chk2 = euclidSub (128 * 12) (128 * 77)
  putStrLn (show chk1)
  putStrLn (show chk2)
  return ()
```

### Rust

```rust
fn euclid_sub(mut a: u64, mut b: u64) -> u64 {
  while a != b {
    if a < b {
      b = b - a;
    } else {
      a = a - b;
    }
  }
  a
}

fn euclid_rem(mut a: u64, mut b: u64) -> u64 {
  while b != 0 {
    let tmp = b;
    b = a % b;
    a = tmp;
  }
  a
}

fn main() {
  let chk1 = euclid_rem(64 * 67, 64 * 81);
  let chk2 = euclid_sub(128 * 12, 128 * 77);
  println!("{}", chk1);
  println!("{}", chk2);
}
```

### OCaml

```ocaml
let rec euclid_mod a b =
  if b = 0 then
    a
  else
    euclid_mod b (a mod b)

let rec euclid_sub a b =
  if a = b then
    a
  else if a < b then
    euclid_sub a (b - a)
  else
    euclid_sub (a - b) b

let chk1 = euclid_mod (64 * 67) (64 * 81)
let chk2 = euclid_sub (128 * 12) (128 * 77)
let () = print_string ((int_of_string chk1) ^ "\n")
let () = print_string ((int_of_string chk2) ^ "\n")
```

### Java
```java
// sumbitted by lolatomroflsinnlos
public static void main(String[] args) {

    System.out.println(euclidSub(64 * 67, 64 * 81));
    System.out.println(euclidMod(128 * 12, 128 * 77));

}

public static int euclidSub(int a, int b) {
    while (a != b) {
        if (a > b) {
            a -=b;
        } else {
            b -=a;
        }
    }

    return a;
}

public static int euclidMod(int a, int b) {
    while (b != 0){
        int temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}﻿
```
