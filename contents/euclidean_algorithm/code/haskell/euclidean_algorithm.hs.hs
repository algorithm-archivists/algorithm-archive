-- contributed by Nicole Mazzuca (ubsan)

--Method 1: Euclid's original subtraction algorithm

euclidSub :: Integer -> Integer -> Integer
euclidSub a b = inner (abs a) (abs b) 
  where
    inner x y
      | x == y = x                      --Exit condition for recursion (seen in next step).
      | x < y = euclidSub x (y - x)     --Recursion with smaller value and difference b/w values as new inputs
      | otherwise = euclidSub (x - y) y --Recursion with smaller value and difference b/w values as new inputs


-- _____________________________________________________________________________________________

--Method 2: Modern implemetation - The modulus method.

euclidMod :: Integer -> Integer -> Integer
euclidMod a b = inner (abs a) (abs b)   
  where
    inner x 0 = x                       --Exit condition for recursion (seen in next step).
    inner x y = inner y (x `mod` y)     --Recursion with b and (a mod b) as new inputs 
                                                -- (with a, b being old inputs)

--___________________________________________________________________________________________

--  Examples

main :: IO ()
main = do
  let chk1 = euclidMod (64 * 67) (64 * 81)
      chk2 = euclidSub (128 * 12) (128 * 77)
  print chk1
  print chk2