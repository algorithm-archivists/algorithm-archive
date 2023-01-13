-- Method 1: Euclid's original subtraction algorithm

euclidSub :: Integer -> Integer -> Integer
euclidSub a b = inner (abs a) (abs b)
  where
    inner x y
      -- if a = b, then the gcd is a
      | x == y = x
      -- if a < b: Recursively call euclidSub with the a and (b-a) as new inputs
      | x < y = euclidSub x (y - x)
      -- otherwise: Recursively call euclidSub with the a and (b-a) as new inputs
      | otherwise = euclidSub (x - y) y

-- _______________________________________________________________________

-- Method 2: Modern implemetation - The modulus method.

euclidMod :: Integer -> Integer -> Integer
euclidMod a b = inner (abs a) (abs b)
  where
    -- if a divides b, then gcd is a
    inner x 0 = x
    -- otherwise, recursively call inner with b and (a mod b) as new inputs
    inner x y = inner y (x `mod` y)

-- _________________________________________________________________________

-- Examples

main :: IO ()
main = do
  let chk1 = euclidMod (64 * 67) (64 * 81)
      chk2 = euclidSub (128 * 12) (128 * 77)
  putStrLn "[#]\nModulus-based euclidean algorithm result:"
  print chk1
  putStrLn "[#]\nSubtraction-based euclidean algorithm result:"
  print chk2
